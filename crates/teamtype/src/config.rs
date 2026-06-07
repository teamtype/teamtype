// SPDX-FileCopyrightText: 2025 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2025 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! Data structures and helper methods around influencing the configuration of the application.

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::Deref;
use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use docstr::docstr;
use git2::{Config as GitConfig, ConfigLevel};
use ini::{Ini, Properties};
use tempfile::{TempDir, tempdir_in};
use tracing::info;

use crate::sandbox;
use crate::setup::find_git_repo;
use crate::setup::get_app_cache_dir;
use crate::types::UserInterface;
use crate::wormhole::get_secret_address_from_wormhole;

pub(crate) const DEFAULT_SOCKET_NAME: &str = "socket";
pub(crate) const CONFIG_DIR: &str = ".teamtype";
pub(crate) const CONFIG_FILE: &str = "config";

const EMIT_JOIN_CODE_DEFAULT: bool = true;
const EMIT_SECRET_ADDRESS_DEFAULT: bool = false;
// TODO: Generate a random funny name.
const USERNAME_FALLBACK: &str = "Anonymous";

#[derive(Clone, Debug)]
pub enum Peer {
    SecretAddress(String),
    JoinCode(String),
}

#[derive(Debug)]
pub enum BaseDir {
    Permanent(PathBuf),
    Temporary(TempDir),
}

impl BaseDir {
    pub fn try_from(path: &Path) -> Result<Self> {
        let base_dir = path.canonicalize().with_context(|| {
            format!(
                "Could not compute the absolute, canonical form of the path of directory {}",
                path.display(),
            )
        })?;
        Ok(Self::Permanent(base_dir))
    }

    pub fn new_temporary() -> Result<Self> {
        let parent_dir = get_app_cache_dir()?;
        let tempdir = tempdir_in(&parent_dir).context(format!(
            "Failed to create a temporary directory in the directory {}",
            parent_dir.display()
        ))?;
        Ok(Self::Temporary(tempdir))
    }
}

impl Deref for BaseDir {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Permanent(path) => path,
            Self::Temporary(temp_dir) => temp_dir.path(),
        }
    }
}

impl Default for BaseDir {
    fn default() -> Self {
        Self::Permanent(PathBuf::from("./"))
    }
}

impl Display for BaseDir {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Permanent(path) => write!(f, "{}", path.display()),
            Self::Temporary(temp_dir) => write!(f, "{}", temp_dir.path().display()),
        }
    }
}

impl Clone for BaseDir {
    fn clone(&self) -> Self {
        match self {
            Self::Permanent(path) => Self::Permanent(path.clone()),
            Self::Temporary(temp_dir) => Self::Permanent(temp_dir.path().to_path_buf()),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Config {
    pub base_dir: BaseDir,
    pub peer: Option<Peer>,
    pub emit_join_code: bool,
    pub emit_secret_address: bool,
    pub magic_wormhole_relay: Option<String>,
    pub iroh_relay: Option<String>,
    pub iroh_dns_domain: Option<String>,
    pub iroh_pkarr_relay: Option<String>,
    // Whether to sync version control directories like .git, .jj, ...
    pub sync_vcs: bool,
    pub username: Option<String>,
}

impl Config {
    // Merges the app config from file with the CLI app config by taking the "superset" of them.
    //
    // It depends on the attribute how we're merging it:
    // - For strings, the CLI app config attribute has precedence.
    // - For booleans, if a value deviates from the default, it "wins".
    // - The `base_dir` will be taken from the CLI app config.
    pub fn from_config_file_and_cli(config_cli: Self, ui: &UserInterface) -> Result<Self> {
        let empty_properties_section = Properties::new();
        let base_dir = config_cli.base_dir;
        let conf;
        let config_file = base_dir.join(CONFIG_DIR).join(CONFIG_FILE);
        let general_section = if config_file.exists() {
            conf = Ini::load_from_file(config_file)
                .context("Could not access config file, even though it exists")?;
            conf.general_section()
        } else {
            &empty_properties_section
        };

        // we do the computation of username before initializing the struct, because we need to
        // reference base_dir, which gets moved into the struct
        let username = get_username(config_cli.username, &base_dir, general_section, ui);
        Ok(Self {
            // TODO: extract all the other fields to its own struct, s.t. we don't have to work
            // around the fact that base_dir won't ever be in the config file.
            base_dir,
            peer: config_cli.peer.or_else(|| {
                general_section
                    .get("peer")
                    .map(|p| Peer::SecretAddress(p.to_string()))
            }),
            emit_join_code: config_cli.emit_join_code
                && general_section
                    .get("emit_join_code")
                    .map_or(EMIT_JOIN_CODE_DEFAULT, |ejc| {
                        ejc.parse()
                            .expect("Failed to parse config parameter `emit_join_code` as bool")
                    }),
            emit_secret_address: config_cli.emit_secret_address
                || general_section.get("emit_secret_address").map_or(
                    EMIT_SECRET_ADDRESS_DEFAULT,
                    |esa| {
                        esa.parse().expect(
                            "Failed to parse config parameter `emit_secret_address` as bool",
                        )
                    },
                ),
            magic_wormhole_relay: config_cli.magic_wormhole_relay.or_else(|| {
                general_section
                    .get("magic_wormhole_relay")
                    .map(ToString::to_string)
            }),
            iroh_relay: config_cli
                .iroh_relay
                .or_else(|| general_section.get("iroh_relay").map(ToString::to_string)),
            iroh_dns_domain: config_cli.iroh_dns_domain.or_else(|| {
                general_section
                    .get("iroh_dns_domain")
                    .map(ToString::to_string)
            }),
            iroh_pkarr_relay: config_cli.iroh_pkarr_relay.or_else(|| {
                general_section
                    .get("iroh_pkarr_relay")
                    .map(ToString::to_string)
            }),
            sync_vcs: config_cli.sync_vcs,
            username: Some(username),
        })
    }

    fn config_file(&self) -> PathBuf {
        self.base_dir.join(CONFIG_DIR).join(CONFIG_FILE)
    }

    /// If we have a join code, try to use that and overwrite the config file.
    /// If we don't have a join code, try to use the configured peer.
    /// Otherwise, fail.
    pub async fn resolve_peer(self) -> Result<Self> {
        let config_file = self.config_file();
        let peer = self.peer;
        let new_peer = match &peer {
            Some(Peer::JoinCode(join_code)) => {
                let secret_address =
                    get_secret_address_from_wormhole(join_code, self.magic_wormhole_relay.clone())
                        .await
                        .context(
                            "Failed to retrieve secret address, was this join code already used?",
                        )?;
                info!(
                    "Derived peer from join code. Storing in config (overwriting previous config)."
                );
                store_peer_in_config(&self.base_dir, &config_file, &secret_address)?;
                Peer::SecretAddress(secret_address)
            }
            Some(Peer::SecretAddress(secret_address)) => {
                info!("Using peer from config file.");
                Peer::SecretAddress(secret_address.clone())
            }
            None => bail!("Missing join code, and no peer=<secret address> in .teamtype/config"),
        };
        Ok(Self {
            peer: Some(new_peer),
            ..self
        })
    }

    #[must_use]
    pub(crate) const fn is_host(&self) -> bool {
        self.peer.is_none()
    }
}

fn store_peer_in_config(base_dir: &BaseDir, config_file: &Path, peer: &str) -> Result<()> {
    info!("Storing peer's address in {}", config_file.display());

    let content = format!("peer={peer}\n");
    sandbox::write_file(base_dir, config_file, content.as_bytes())
        .context("Failed to write to config file")
}

#[must_use]
pub(crate) fn has_git_remote(base_dir: &BaseDir) -> bool {
    if let Ok(repo) = find_git_repo(base_dir)
        && let Ok(remotes) = repo.remotes()
    {
        return !remotes.is_empty();
    }
    false
}

/// Test if the local Git config has *any* user config.
pub(crate) fn has_local_user_config(base_dir: &BaseDir) -> Result<bool> {
    let snapshot = find_git_repo(base_dir)?.config()?.snapshot()?;
    let mut entries = snapshot.entries(Some("user\\."))?;
    while let Some(Ok(entry)) = entries.next() {
        match entry.level() {
            ConfigLevel::Local | ConfigLevel::Worktree | ConfigLevel::App => return Ok(true),
            _ => {}
        }
    }
    Ok(false)
}

fn get_username(
    config_cli_username: Option<String>,
    base_dir: &BaseDir,
    general_section: &Properties,
    ui: &UserInterface,
) -> String {
    config_cli_username
        .map(|u| get_username_from_cli(u, ui))
        .or_else(|| get_username_from_config_file(general_section, ui))
        .or_else(|| get_username_from_git(base_dir, ui))
        .unwrap_or_else(|| get_username_from_fallback_value(ui))
}

fn get_username_from_cli(username: String, ui: &UserInterface) -> String {
    info!("Using the CLI provided username '{username}'");
    ui.inform(&format!(
        "Using the CLI provided username '{username}' to display next to the cursors other people see."
    ));
    username
}

fn get_username_from_config_file(
    general_section: &Properties,
    ui: &UserInterface,
) -> Option<String> {
    general_section
        .get("username")
        .map(ToString::to_string)
        .map(|username| {
            info!("Using the username '{username}' from `.teamtype/config` as username");
            ui.inform(&format!(
                "Using the username '{username}' from `.teamtype/config` as username, to display next to the cursors other people see."
            ));
            username
        })
}

fn get_username_from_git(base_dir: &BaseDir, ui: &UserInterface) -> Option<String> {
    let username = get_git_username(base_dir);
    if let Some(ref username) = username {
        info!("Using the Git username '{username}' as username");
        ui.inform(&docstr!(format!
                /// Using the Git username '{username}' as username, to display next to the cursors other people see.
                /// Teamtype uses the Git username as username by default.
                /// You can set the configuration value `username` in your `.teamtype/config` to override this username.
                /// You can also use the flag `--username` when using the `share`/`join` subcommands.
        ));
    }
    username
}

fn get_username_from_fallback_value(ui: &UserInterface) -> String {
    let username = USERNAME_FALLBACK.to_string();
    info!("Using the fallback value for username '{username}' as username");
    ui.inform(&docstr!(format!
        /// Using the fallback value for username '{username}' as username, to display next to the cursors other people see.
        /// You can set the configuration value `username` in your `.teamtype/config` to override this username.
        /// You can also use the flag `--username` when using the `share`/`join` subcommands.
    ));
    username
}

#[must_use]
fn get_git_username(base_dir: &BaseDir) -> Option<String> {
    local_git_username(base_dir)
        .or_else(|_| global_git_username())
        .ok()
        .filter(|username| !username.is_empty()) // If the username is empty, return None. This can
    // happen if Git is installed, but no username is
    // set on any level of Git configuration.
}

fn local_git_username(base_dir: &BaseDir) -> Result<String> {
    Ok(find_git_repo(base_dir)?
        .config()?
        .snapshot()?
        .get_str("user.name")?
        .to_string())
}

fn global_git_username() -> Result<String> {
    Ok(GitConfig::open_default()?
        .snapshot()?
        .get_str("user.name")?
        .to_string())
}
