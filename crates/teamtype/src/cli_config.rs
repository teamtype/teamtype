// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env::current_dir;
use std::path::PathBuf;

use anyhow::{Context, Result};
use teamtype::config::{self, AppConfig};
use teamtype::types::UserInterface;

use super::cli::{Cli, Commands, ShareJoinFlags};

pub async fn parse_join_config(
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

pub fn parse_share_config(command: Commands, base_dir: PathBuf, ui: &UserInterface) -> AppConfig {
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

pub fn parse_directory_config(cli: &Cli) -> Result<Option<PathBuf>> {
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
