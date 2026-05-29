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
use teamtype::types::UserInterface;
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

    // Determine if the CLI flags request proceeding with a temporary directory, some user
    // specified directory, or fallback to just the current directory.
    let directory = parse_directory_config(&cli)?;

    // Now that we know what the base directory is going to be, either validate our access to it and
    // an existing config therein or setup a new config. In the event this step creates a temporary
    // directory we need to hang onto the handle as long as we're running.
    let (base_dir, _tempdir) = setup_teamtype_directory(directory.as_deref(), ui)
        .context("Failed to find .teamtype/ directory")?;

    // TODO: If the result of this joined future handles were to go out of scope and hence be
    // dropped, *some* but not all parts of the daemon would shut down. Notably the local socket
    // will go away but the remote networking connections would stay up! This is a fundamental issue
    // with the daemon module and merits refactoring so the long running socket and network
    // listeners properly listen to the mpsc channel or receive and process a cancellation token.
    let _handle = match cli.command {
        Commands::Client => return run_client(base_dir).await,
        Commands::Join { .. } => {
            let join_config = parse_join_config(cli.command, base_dir, ui).await?;
            run_daemon(join_config, false, ui).await
        }
        Commands::Share { init, .. } => {
            let share_config = parse_share_config(cli.command, base_dir, ui);
            run_daemon(share_config, init, ui).await
        }
    }?;

    trap_shutdown().await;

    Ok(())
}

async fn parse_join_config(
    command: Commands,
    base_dir: PathBuf,
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
            base_dir,
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

fn parse_share_config(command: Commands, base_dir: PathBuf, ui: &UserInterface) -> AppConfig {
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
            base_dir,
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

fn parse_directory_config(cli: &Cli) -> Result<Option<PathBuf>> {
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
                return Ok(None);
            }
        }
        Commands::Client => {}
    }
    let directory = match cli.directory {
        Some(ref directory) => directory,
        None => &current_dir().context("Could not access current directory")?,
    };
    Ok(Some(directory.clone()))
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
