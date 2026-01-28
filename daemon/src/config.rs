// SPDX-FileCopyrightText: 2025 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2025 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! Data structures and helper methods around influencing the configuration of the application.
use crate::sandbox;
use crate::wormhole::get_secret_address_from_wormhole;
use anyhow::{bail, Context, Result};
use git2::ConfigLevel;
use ini::{Ini, Properties};
use std::path::{Path, PathBuf};
use tracing::info;

pub const DOC_FILE: &str = "doc";
pub const DEFAULT_SOCKET_NAME: &str = "socket";
pub const CONFIG_DIR: &str = ".teamtype";
pub const CONFIG_FILE: &str = "config";
pub const BOOKMARK_FILE: &str = "bookmark";
// TODO: Remove this after a while.
pub const LEGACY_CONFIG_DIR: &str = ".ethersync";

const EMIT_JOIN_CODE_DEFAULT: bool = true;
const EMIT_SECRET_ADDRESS_DEFAULT: bool = false;
// TODO: Generate a random funny name.
const USERNAME_FALLBACK: &str = "Anonymous";

#[derive(Clone, Debug)]
pub enum Peer {
    SecretAddress(String),
    JoinCode(String),
}

#[derive(Clone, Default, Debug)]
#[must_use]
pub struct AppConfig {
    pub base_dir: PathBuf,
    pub peer: Option<Peer>,
    pub emit_join_code: bool,
    pub emit_secret_address: bool,
    pub magic_wormhole_relay: Option<String>,
    // Whether to sync version control directories like .git, .jj, ...
    pub sync_vcs: bool,
    pub username: Option<String>,
}

impl AppConfig {
    // Merges the app config from file with the CLI app config by taking the "superset" of them.
    //
    // It depends on the attribute how we're merging it:
    // - For strings, the CLI app config attribute has precedence.
    // - For booleans, if a value deviates from the default, it "wins".
    // - The `base_dir` will be taken from the CLI app config.
    pub fn from_config_file_and_cli(app_config_cli: Self) -> Self {
        let base_dir = app_config_cli.base_dir;
        let config_file = base_dir.join(CONFIG_DIR).join(CONFIG_FILE);
        let empty_properties_section = Properties::new();
        let conf;
        let general_section = if config_file.exists() {
            conf = Ini::load_from_file(config_file)
                .expect("Could not access config file, even though it exists");
            conf.general_section()
        } else {
            &empty_properties_section
        };

        // we do the computation of username before initializing the struct, because we need to
        // reference base_dir, which gets moved during the initialization of the struct
        let username = get_username(app_config_cli.username, &base_dir, general_section);
        Self {
            // TODO: extract all the other fields to its own struct, s.t. we don't have to work
            // around the fact that base_dir won't ever be in the config file.
            base_dir,
            peer: app_config_cli.peer.or_else(|| {
                general_section
                    .get("peer")
                    .map(|p| Peer::SecretAddress(p.to_string()))
            }),
            emit_join_code: app_config_cli.emit_join_code
                && general_section
                    .get("emit_join_code")
                    .map_or(EMIT_JOIN_CODE_DEFAULT, |ejc| {
                        ejc.parse()
                            .expect("Failed to parse config parameter `emit_join_code` as bool")
                    }),
            emit_secret_address: app_config_cli.emit_secret_address
                || general_section.get("emit_secret_address").map_or(
                    EMIT_SECRET_ADDRESS_DEFAULT,
                    |esa| {
                        esa.parse().expect(
                            "Failed to parse config parameter `emit_secret_address` as bool",
                        )
                    },
                ),
            magic_wormhole_relay: app_config_cli.magic_wormhole_relay.or_else(|| {
                general_section
                    .get("magic_wormhole_relay")
                    .map(ToString::to_string)
            }),
            sync_vcs: app_config_cli.sync_vcs,
            username: Some(username),
        }
    }

    fn config_file(&self) -> PathBuf {
        self.base_dir.join(CONFIG_DIR).join(CONFIG_FILE)
    }

    /// If we have a join code, try to use that and overwrite the config file.
    /// If we don't have a join code, try to use the configured peer.
    /// Otherwise, fail.
    pub async fn resolve_peer(self) -> Result<Self> {
        let mut result = self.clone();
        let peer = match self.peer {
            Some(Peer::JoinCode(ref join_code)) => {
                let secret_address =
                    get_secret_address_from_wormhole(join_code, self.magic_wormhole_relay.clone())
                        .await
                        .context(
                            "Failed to retreive secret address, was this join code already used?",
                        )?;
                info!(
                    "Derived peer from join code. Storing in config (overwriting previous config)."
                );
                store_peer_in_config(&self.base_dir, &self.config_file(), &secret_address)?;
                Peer::SecretAddress(secret_address)
            }
            Some(Peer::SecretAddress(secret_address)) => {
                info!("Using peer from config file.");
                Peer::SecretAddress(secret_address)
            }
            None => {
                bail!("Missing join code, and no peer=<secret address> in .teamtype/config");
            }
        };
        result.peer = Some(peer);
        Ok(result)
    }

    #[must_use]
    pub const fn is_host(&self) -> bool {
        self.peer.is_none()
    }
}

pub fn store_peer_in_config(directory: &Path, config_file: &Path, peer: &str) -> Result<()> {
    info!("Storing peer's address in .teamtype/config.");

    let content = format!("peer={peer}\n");
    sandbox::write_file(directory, config_file, content.as_bytes())
        .context("Failed to write to config file")
}

#[must_use]
pub fn has_git_remote(path: &Path) -> bool {
    if let Ok(repo) = find_git_repo(path) {
        if let Ok(remotes) = repo.remotes() {
            return !remotes.is_empty();
        }
    }
    false
}

#[must_use]
fn teamtype_directory_should_be_ignored_but_isnt(path: &Path) -> bool {
    if let Ok(repo) = find_git_repo(path) {
        let teamtype_dir = path.join(CONFIG_DIR);
        return !repo
            .is_path_ignored(teamtype_dir)
            .expect("Should have been able to determine ignore state of path");
    }
    false
}

/// Test if the local Git config has *any* user config.
pub fn has_local_user_config(base_dir: &Path) -> Result<bool> {
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
    app_config_cli_username: Option<String>,
    base_dir: &Path,
    general_section: &Properties,
) -> String {
    app_config_cli_username
        .map(get_username_from_cli)
        .or_else(|| get_username_from_config_file(general_section))
        .or_else(|| get_username_from_git(base_dir))
        .unwrap_or_else(get_username_from_fallback_value)
}

fn get_username_from_cli(username: String) -> String {
    info!("Using the username '{username}' to display next to the cursors other people see.");
    username
}

fn get_username_from_config_file(general_section: &Properties) -> Option<String> {
    general_section
        .get("username")
        .map(ToString::to_string)
        .map(|username| {
            info!("Using the username '{username}' from `.teamtype/config` as username, to display next to the cursors other people see.");
            username
        })
}

fn get_username_from_git(base_dir: &Path) -> Option<String> {
    let username = get_git_username(base_dir);
    if let Some(ref username) = username {
        info!("Using the Git username '{username}' as username, to display next to the cursors other people see.");
        info!("Teamtype uses the Git username as username by default.");
        info!("You can set the configuration value `username` in your `.teamtype/config` to override this username.");
        info!("You can also use the flag `--username` when using the `share`/`join` subcommands.");
    }
    username
}

fn get_username_from_fallback_value() -> String {
    let username = USERNAME_FALLBACK.to_string();
    info!("Using the fallback value for username '{username}' as username, to display next to the cursors other people see.");
    info!("You can set the configuration value `username` in your `.teamtype/config` to override this username.");
    info!("You can also use the flag `--username` when using the `share`/`join` subcommands.");
    username
}

#[must_use]
pub fn get_git_username(base_dir: &Path) -> Option<String> {
    local_git_username(base_dir)
        .or_else(|_| global_git_username())
        .ok()
        .filter(|username| !username.is_empty()) // If the username is empty, return None. This can
                                                 // happen if Git is installed, but no username is
                                                 // set on any level of Git configuration.
}

fn local_git_username(base_dir: &Path) -> Result<String> {
    Ok(find_git_repo(base_dir)?
        .config()?
        .snapshot()?
        .get_str("user.name")?
        .to_string())
}

fn global_git_username() -> Result<String> {
    Ok(git2::Config::open_default()?
        .snapshot()?
        .get_str("user.name")?
        .to_string())
}

fn find_git_repo(path: &Path) -> Result<git2::Repository, git2::Error> {
    git2::Repository::discover(path)
}

fn add_teamtype_to_local_gitignore(directory: &Path) -> Result<()> {
    let mut ignore_file_path = directory.join(CONFIG_DIR);
    ignore_file_path.push(".gitignore");

    // It's very unlikely that .teamtype/.gitignore will already contain something, but let's
    // still append.
    let bytes_in = sandbox::read_file(directory, &ignore_file_path).unwrap_or_default();
    // TODO: use String::from_utf8
    let mut content = std::str::from_utf8(&bytes_in)?.to_string();

    if !content.is_empty() && !content.ends_with('\n') {
        content.push('\n');
    }
    content.push_str("/*\n");
    let bytes_out = content.as_bytes();
    sandbox::write_file(directory, &ignore_file_path, bytes_out)?;

    Ok(())
}

pub fn ensure_teamtype_is_ignored(directory: &Path) -> Result<()> {
    if teamtype_directory_should_be_ignored_but_isnt(directory) {
        add_teamtype_to_local_gitignore(directory)?;
    }
    Ok(())
}
