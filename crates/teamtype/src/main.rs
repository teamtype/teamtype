// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::panic;
use std::process::exit;

use anyhow::{Context, Result};
use clap::{CommandFactory as _, FromArgMatches as _};
use teamtype::client::run_client;
use teamtype::daemon::run_daemon;
use teamtype::logging;
use teamtype::types::UserInterface;
use tokio::signal;
use tracing::info;

mod cli;
mod cli_config;
mod cli_ui;

use self::cli::{Cli, Commands};
use self::cli_config::parse_client_config;
use self::cli_config::parse_join_config;
use self::cli_config::parse_share_config;
use self::cli_ui::ConsoleInteractions;

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

    // TODO: If the result of this joined future handles were to go out of scope and hence be
    // dropped, *some* but not all parts of the daemon would shut down. Notably the local socket
    // will go away but the remote networking connections would stay up! This is a fundamental issue
    // with the daemon module and merits refactoring so the long running socket and network
    // listeners properly listen to the mpsc channel or receive and process a cancellation token.
    let _handle = match cli.command {
        Commands::Client => {
            let client_config = parse_client_config(cli, ui)?;
            return run_client(client_config).await;
        }
        Commands::Join { .. } => {
            let join_config = parse_join_config(cli, ui)?;
            run_daemon(join_config, false, ui).await
        }
        Commands::Share { init, .. } => {
            let share_config = parse_share_config(cli, ui)?;
            run_daemon(share_config, init, ui).await
        }
    }?;

    trap_shutdown().await;

    Ok(())
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
