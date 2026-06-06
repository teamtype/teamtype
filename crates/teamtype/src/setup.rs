// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use docstr::docstr;
use git2::{Error as GitError, Repository};
use microxdg::XdgApp;
use tempfile::{TempDir, tempdir_in};

use crate::config::CONFIG_DIR;
use crate::config::Config;
use crate::sandbox;
use crate::types::UserInterface;

// Once we know what the base directory is going to be, either validate our access to it and
// an existing config therein or setup a new config. In the event this step creates a temporary
// directory we need to hang onto the handle as long as we're running.
pub fn setup_teamtype_directory(
    config: Config,
    ui: &UserInterface,
) -> Result<(Config, Option<TempDir>)> {
    let mut config = config;
    let directory = config.base_dir;
    let (base_dir, temp_dir) = if let Some(directory) = directory.as_ref() {
        let base_dir = directory.canonicalize().with_context(|| {
            format!(
                "Could not compute the absolute, canonical form of the path of directory {}",
                directory.display(),
            )
        })?;
        (base_dir, None)
    } else {
        let temp_dir = setup_temporary_directory()?;
        (temp_dir.path().to_path_buf(), Some(temp_dir))
    };
    if !has_teamtype_directory(&base_dir) {
        let teamtype_dir = base_dir.join(CONFIG_DIR);
        if ui.confirm(&docstr!(format!
            /// '{}' hasn't been used as a Teamtype directory before.
            ///
            /// Do you want to enable live collaboration here? (This will create a {CONFIG_DIR}/ directory.)
            base_dir.display(),
        ))? {
            sandbox::create_dir(&base_dir, &teamtype_dir)?;
            ui.log("Created! Resuming launch.");
        } else {
            bail!("Aborting launch. Teamtype needs a {CONFIG_DIR}/ directory to function");
        }
    }
    config.base_dir = Some(base_dir);
    Ok((config, temp_dir))
}

fn has_teamtype_directory(dir: &Path) -> bool {
    let teamtype_dir = dir.join(CONFIG_DIR);
    // Using the sandbox method here is technically unnecessary,
    // but we want to really run all path operations through the sandbox module.
    sandbox::exists(dir, &teamtype_dir).expect("Failed to check") && teamtype_dir.is_dir()
}

pub fn setup_temporary_directory() -> Result<TempDir> {
    let parent_dir = get_app_cache_dir()?;
    tempdir_in(&parent_dir).with_context(|| {
        format!(
            "Failed to create a temporary directory in the directory {}",
            parent_dir.display()
        )
    })
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

pub(crate) fn ensure_teamtype_is_ignored(directory: &Path) -> Result<()> {
    if teamtype_directory_should_be_ignored_but_isnt(directory) {
        add_teamtype_to_local_gitignore(directory)?;
    }
    Ok(())
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

pub(crate) fn find_git_repo(path: &Path) -> Result<Repository, GitError> {
    Repository::discover(path)
}
