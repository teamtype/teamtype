// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use docstr::docstr;
use git2::{Error as GitError, Repository};
use microxdg::XdgApp;

use crate::config::CONFIG_DIR;
use crate::config::ProjectDir;
use crate::sandbox;
use crate::types::UserInterface;

// Once we know what the project directory is going to be, either validate our access to it and
// an existing config therein or setup a new config. In the event this step creates a temporary
// directory we need to hang onto the handle as long as we're running.
pub fn setup_teamtype_directory(project_dir: &ProjectDir, ui: &UserInterface) -> Result<()> {
    if !has_teamtype_directory(project_dir) {
        let teamtype_dir = project_dir.join(CONFIG_DIR);
        match project_dir {
            ProjectDir::Temporary(_) => {
                ui.log(&format!(
                    "'{}' is the temporary directory that is used as a Teamtype directory.",
                    project_dir.display()
                ));
                sandbox::create_dir(project_dir, &teamtype_dir)?;
            }
            ProjectDir::Permanent(_) => {
                if ui.confirm(&docstr!(format!
                    /// '{}' hasn't been used as a Teamtype directory before.
                    ///
                    /// Do you want to enable live collaboration here? (This will create a {CONFIG_DIR}/ directory.)
                    project_dir.display(),
                ))? {
                    sandbox::create_dir(project_dir, &teamtype_dir)?;
                    ui.log("Created! Resuming launch.");
                } else {
                    bail!("Aborting launch. Teamtype needs a {CONFIG_DIR}/ directory to function");
                }
            }
        }
    }
    Ok(())
}

fn has_teamtype_directory(dir: &Path) -> bool {
    let teamtype_dir = dir.join(CONFIG_DIR);
    // Using the sandbox method here is technically unnecessary,
    // but we want to really run all path operations through the sandbox module.
    sandbox::exists(dir, &teamtype_dir).expect("Failed to check") && teamtype_dir.is_dir()
}

pub(crate) fn get_app_cache_dir() -> Result<PathBuf> {
    // The XDG base directory specification doesn't apply on Windows, and the microxdg crate only
    // looks for the `HOME` & `USER` environment variables, which standard Windows shells don't set.
    // Use the Windows standard per-user temporary directory (based on '%TEMP%') instead.
    if cfg!(windows) {
        return Ok(std::env::temp_dir());
    }

    let xdg = XdgApp::new("teamtype").context("Unable to create XDG app namespace")?;
    let app_cache_dir = xdg
        .app_cache()
        .context("Unable to resolve XDG app cache dir")?;
    let app_cache_dir_parent = app_cache_dir.parent().with_context(|| {
        format!(
            "Failed to get parent directory of the directory {}",
            app_cache_dir.display()
        )
    })?;
    // Using the sandbox method here is technically unnecessary,
    // but we want to really run all path operations through the sandbox module.
    sandbox::create_dir_all(app_cache_dir_parent, &app_cache_dir)
        .context("Unable to create app cache dir using sandbox")?;
    Ok(app_cache_dir)
}

pub(crate) fn ensure_teamtype_is_ignored(project_dir: &ProjectDir) -> Result<()> {
    if teamtype_directory_should_be_ignored_but_isnt(project_dir) {
        add_teamtype_to_local_gitignore(project_dir)?;
    }
    Ok(())
}

#[must_use]
fn teamtype_directory_should_be_ignored_but_isnt(project_dir: &ProjectDir) -> bool {
    if let Ok(repo) = find_git_repo(project_dir) {
        let teamtype_dir = project_dir.join(CONFIG_DIR);
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

pub(crate) fn find_git_repo(project_dir: &ProjectDir) -> Result<Repository, GitError> {
    Repository::discover(project_dir)
}
