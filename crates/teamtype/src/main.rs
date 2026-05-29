// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env::current_dir;
use std::panic;
use std::path::PathBuf;
use std::process::exit;

use anyhow::{Context, Result};
use clap::{CommandFactory as _, FromArgMatches as _};
use teamtype::client::run_client;
use teamtype::config::{self, AppConfig};
use teamtype::daemon::run_daemon;
use teamtype::logging;
use teamtype::setup::setup_teamtype_directory;
use teamtype::setup::setup_temporary_directory;
use teamtype::types::UserInterface;
use tempfile::TempDir;
use tokio::signal;
use tracing::info;

use self::cli::{Cli, Commands, ShareJoinFlags};

mod cli;
mod cli_ui;

use cli_ui::ConsoleInteractions;

#[tokio::main]
async fn main() -> Result<()> {
    let default_panic = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        default_panic(info);
        exit(1);
    }));

    let arg_matches = Cli::command().get_matches();
    let cli = match Cli::from_arg_matches(&arg_matches) {
        Ok(cli) => cli,
        Err(e) => e.exit(),
    };

    logging::initialize(cli.verbose).context("Failed to initialize logging")?;

    let ui = &UserInterface::wrap(ConsoleInteractions {});

    let temporary_directory = get_temporary_directory(&cli)?;
    let directory = get_directory(temporary_directory.as_ref(), &cli)?;
    setup_teamtype_directory(&directory, temporary_directory.as_ref(), ui)
        .context("Failed to find .teamtype/ directory")?;

    // TODO: If the result of this joined future handles were to go out of scope and hence be
    // dropped, *some* but not all parts of the daemon would shut down. Notably the local socket
    // will go away but the remote networking connections would stay up! This is a fundamental issue
    // with the daemon module and merits refactoring so the long running socket and network
    // listeners properly listen to the mpsc channel or receive and process a cancellation token.
    let _handle = match cli.command {
        Commands::Client => return run_client(directory.clone()).await,
        Commands::Join { .. } => {
            let join_config = parse_join_config(cli.command, directory.clone(), ui).await?;
            run_daemon(join_config, false, ui).await
        }
        Commands::Share { init, .. } => {
            let share_config = parse_share_config(cli.command, directory.clone(), ui);
            run_daemon(share_config, init, ui).await
        }
    }?;

    trap_shutdown().await;

    Ok(())
}

async fn parse_join_config(
    command: Commands,
    directory: PathBuf,
    ui: &UserInterface,
) -> Result<AppConfig> {
    if let Commands::Join {
        join_code,
        shared_flags:
            ShareJoinFlags {
                magic_wormhole_relay,
                iroh_relay,
                iroh_dns_domain,
                iroh_pkarr_relay,
                sync_vcs,
                username,
                ..
            },
        ..
    } = command
    {
        let app_config_cli = AppConfig {
            base_dir: directory,
            peer: join_code.map(config::Peer::JoinCode),
            emit_join_code: false,
            emit_secret_address: false,
            magic_wormhole_relay,
            iroh_relay,
            iroh_dns_domain,
            iroh_pkarr_relay,
            sync_vcs,
            username,
        };
        let mut app_config = AppConfig::from_config_file_and_cli(app_config_cli, ui);
        app_config = app_config
            .resolve_peer()
            .await
            .context("Failed to resolve peer")?;
        Ok(app_config)
    } else {
        unreachable!("Only Join commands beget Join configs.")
    }
}

fn parse_share_config(command: Commands, directory: PathBuf, ui: &UserInterface) -> AppConfig {
    if let Commands::Share {
        no_join_code,
        shared_flags:
            ShareJoinFlags {
                magic_wormhole_relay,
                iroh_relay,
                iroh_dns_domain,
                iroh_pkarr_relay,
                sync_vcs,
                username,
                ..
            },
        show_secret_address,
        ..
    } = command
    {
        let app_config_cli = AppConfig {
            base_dir: directory,
            peer: None,
            emit_join_code: !no_join_code,
            emit_secret_address: show_secret_address,
            magic_wormhole_relay,
            iroh_relay,
            iroh_dns_domain,
            iroh_pkarr_relay,
            sync_vcs,
            username,
        };
        let mut app_config = AppConfig::from_config_file_and_cli(app_config_cli, ui);
        // Because of the "share" subcommand, explicitly don't connect anywhere.
        app_config.peer = None;
        app_config
    } else {
        unreachable!("Only Share commands beget Share configs.")
    }
}

async fn trap_shutdown() {
    let interruption = async {
        signal::ctrl_c()
            .await
            .expect("unable to setup Ctrl+C handler");
    };

    #[cfg(unix)]
    let termination = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("unable to setup SIGTERM handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let termination = std::future::pending::<()>();

    tokio::select! {
        () = interruption => {
            info!("Got SIGINT (Ctrl+C), shutting down");
        }
        () = termination => {
            info!("Got SIGTERM, shutting down");
        }
    }
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
                Ok(Some(setup_temporary_directory()?))
            } else {
                Ok(None)
            }
        }
        Commands::Client => Ok(None),
    }
}

fn get_directory(temporary_directory: Option<&TempDir>, cli: &Cli) -> Result<PathBuf> {
    let directory = match temporary_directory {
        Some(temporary_directory) => &temporary_directory.path().to_path_buf(),
        None => match cli.directory {
            Some(ref directory) => directory,
            None => &current_dir().context("Could not access current directory")?,
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
