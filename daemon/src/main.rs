// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use self::cli::{Cli, Commands, ShareJoinFlags};
use anyhow::{bail, Context, Result};
use clap::{CommandFactory as _, FromArgMatches as _};
use microxdg::XdgApp;
use std::path::{Path, PathBuf};
use teamtype::{
    cli_ask::ask,
    config::{self, AppConfig},
    daemon::Daemon,
    logging, sandbox,
};
use tempfile::{tempdir_in, TempDir};
use tokio::signal;
use tracing::{debug, info, warn};

mod cli;
mod jsonrpc_forwarder;

fn has_ethersync_directory(dir: &Path) -> bool {
    let ethersync_dir = dir.join(config::LEGACY_CONFIG_DIR);
    // Using the sandbox method here is technically unnecessary,
    // but we want to really run all path operations through the sandbox module.
    sandbox::exists(dir, &ethersync_dir).expect("Failed to check") && ethersync_dir.is_dir()
}

fn has_teamtype_directory(dir: &Path) -> bool {
    let teamtype_dir = dir.join(config::CONFIG_DIR);
    // Using the sandbox method here is technically unnecessary,
    // but we want to really run all path operations through the sandbox module.
    sandbox::exists(dir, &teamtype_dir).expect("Failed to check") && teamtype_dir.is_dir()
}

#[tokio::main]
async fn main() -> Result<()> {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        default_panic(info);
        std::process::exit(1);
    }));

    let arg_matches = Cli::command().get_matches();
    let cli = match Cli::from_arg_matches(&arg_matches) {
        Ok(cli) => cli,
        Err(e) => e.exit(),
    };

    logging::initialize().context("Failed to initialize logging")?;

    let temporary_directory = get_temporary_directory(&cli)?;
    let directory = get_directory(temporary_directory.as_ref(), &cli)?;
    setup_teamtype_directory(&directory, temporary_directory.as_ref())
        .context("Failed to find .teamtype/ directory")?;

    let socket_path = directory
        .join(config::CONFIG_DIR)
        .join(config::DEFAULT_SOCKET_NAME);

    match cli.command {
        Commands::Share { .. } | Commands::Join { .. } => {
            let persist = !config::has_git_remote(&directory);
            if !persist {
                // TODO: drop .teamtype/doc here? Would that be rude?
                info!(
                    "Detected a Git remote: Assuming a pair-programming use-case and starting a new history."
                );
            }

            config::ensure_teamtype_is_ignored(&directory)?;

            let mut init_doc = false;
            let mut app_config;

            match cli.command {
                Commands::Share {
                    init,
                    no_join_code,
                    shared_flags:
                        ShareJoinFlags {
                            magic_wormhole_relay,
                            sync_vcs,
                            username,
                            ..
                        },
                    show_secret_address,
                    ..
                } => {
                    init_doc = init;

                    let app_config_from_config_file = AppConfig::from_config_file(&directory);

                    let app_config_cli = AppConfig {
                        base_dir: directory,
                        peer: None,
                        emit_join_code: !no_join_code,
                        emit_secret_address: show_secret_address,
                        magic_wormhole_relay,
                        sync_vcs,
                        username,
                    };
                    app_config = app_config_cli.merge(app_config_from_config_file);

                    // Because of the "share" subcommand, explicitly don't connect anywhere.
                    app_config.peer = None;
                }
                Commands::Join {
                    join_code,
                    shared_flags:
                        ShareJoinFlags {
                            magic_wormhole_relay,
                            sync_vcs,
                            username,
                            ..
                        },
                    ..
                } => {
                    let app_config_from_config_file = AppConfig::from_config_file(&directory);

                    let app_config_cli = AppConfig {
                        base_dir: directory,
                        peer: join_code.map(config::Peer::JoinCode),
                        emit_join_code: false,
                        emit_secret_address: false,
                        magic_wormhole_relay,
                        sync_vcs,
                        username,
                    };

                    app_config = app_config_cli.merge(app_config_from_config_file);

                    app_config = app_config
                        .resolve_peer()
                        .await
                        .context("Failed to resolve peer")?;
                }
                Commands::Client => {
                    panic!("This can't happen, as we earlier matched on Share|Join.")
                }
            }

            if app_config.sync_vcs
                && config::has_local_user_config(&app_config.base_dir).is_ok_and(|v| v)
            {
                warn!("You have a local user configuration in your .git/config. In --sync-vcs mode, this file will also be synchronized between peers. If your version \"wins\", all peers will have the same Git identity. As a workaround, you could use `git commit --author`.");
            }

            debug!("Starting Teamtype on {}.", app_config.base_dir.display());

            // TODO: Derive socket_path inside the constructor.
            let _daemon = Daemon::new(app_config, &socket_path, init_doc, persist)
                .await
                .context("Failed to launch the daemon")?;
            wait_for_shutdown().await;
        }
        Commands::Client => {
            jsonrpc_forwarder::connection(&socket_path)
                .await
                .context("JSON-RPC forwarder failed")?;
        }
    }
    Ok(())
}

fn get_temporary_directory(cli: &Cli) -> Result<Option<TempDir>> {
    match cli.command {
        Commands::Share {
            shared_flags:
                ShareJoinFlags {
                    temporary_directory,
                    ..
                },
            ..
        }
        | Commands::Join {
            shared_flags:
                ShareJoinFlags {
                    temporary_directory,
                    ..
                },
            ..
        } => {
            if temporary_directory {
                let temporary_directory_parent_directory = &get_app_cache_dir()?;
                Ok(Some(
                    tempdir_in(temporary_directory_parent_directory).with_context(|| {
                        format!(
                            "Failed to create a temporary directory in the directory {}",
                            temporary_directory_parent_directory.display()
                        )
                    })?,
                ))
            } else {
                Ok(None)
            }
        }
        Commands::Client => Ok(None),
    }
}

fn get_app_cache_dir() -> Result<PathBuf> {
    let xdg = XdgApp::new("teamtype")?;
    let app_cache_dir = xdg.app_cache()?;
    let app_cache_dir_parent = app_cache_dir.parent().with_context(|| {
        format!(
            "Failed to get parent directory of the directory {}",
            app_cache_dir.display()
        )
    })?;
    // Using the sandbox method here is technically unnecessary,
    // but we want to really run all path operations through the sandbox module.
    sandbox::create_dir(app_cache_dir_parent, &app_cache_dir)?;
    Ok(app_cache_dir)
}

fn get_directory(temporary_directory: Option<&TempDir>, cli: &Cli) -> Result<PathBuf> {
    let directory = match temporary_directory {
        Some(temporary_directory) => &temporary_directory.path().to_path_buf(),
        None => match cli.directory {
            Some(ref directory) => directory,
            None => &get_current_directory()?,
        },
    };
    let directory = directory.canonicalize().with_context(|| {
        format!(
            "Could not compute the absolute, canonical form of the path of directory {}",
            directory.display(),
        )
    })?;
    Ok(directory)
}

fn setup_teamtype_directory(directory: &Path, temporary_directory: Option<&TempDir>) -> Result<()> {
    if has_ethersync_directory(directory) {
        let old_directory = directory.join(config::LEGACY_CONFIG_DIR);

        warn!("You have an '{}/' directory, back from when the project was called \"Ethersync\" until October 2025.", &old_directory.display());

        if ask(&format!(
            "Do you want to rename {}/ to {}/?",
            &config::LEGACY_CONFIG_DIR,
            &config::CONFIG_DIR,
        ))? {
            let new_directory = directory.join(config::CONFIG_DIR);
            sandbox::rename_file(directory, &old_directory, &new_directory)?;
        } else {
            bail!(
                "Aborting launch. Rename or remove the {} directory yourself to continue.",
                &config::LEGACY_CONFIG_DIR
            );
        }
    }
    if !has_teamtype_directory(directory) {
        let teamtype_dir = directory.join(config::CONFIG_DIR);
        let directory_is_temporary_directory = temporary_directory.is_some();
        if directory_is_temporary_directory {
            info!(
                "'{}' is the temporary directory that is used as a Teamtype directory.",
                &directory.display()
            );
            sandbox::create_dir(directory, &teamtype_dir)?;
        } else {
            warn!(
                "'{}' hasn't been used as a Teamtype directory before.",
                &directory.display()
            );

            if ask(&format!(
                "Do you want to enable live collaboration here? (This will create an {}/ directory.)",
                config::CONFIG_DIR
            ))? {
                sandbox::create_dir(directory, &teamtype_dir)?;
                info!("Created! Resuming launch.");
            } else {
                bail!("Aborting launch. Teamtype needs a .teamtype/ directory to function");
            }
        }
    }
    Ok(())
}

fn get_current_directory() -> Result<PathBuf> {
    std::env::current_dir().context("Could not access current directory")
}

async fn wait_for_shutdown() {
    let mut signal_terminate = signal::unix::signal(signal::unix::SignalKind::terminate())
        .expect("Should have been able to create terminate signal stream");
    tokio::select! {
        _ = signal::ctrl_c() => {
            debug!("Got SIGINT (Ctrl+C), shutting down");
        }
        _ = signal_terminate.recv() => {
            debug!("Got SIGTERM, shutting down");
        }
    }
}
