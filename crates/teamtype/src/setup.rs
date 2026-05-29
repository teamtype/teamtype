// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use microxdg::XdgApp;
use tempfile::{TempDir, tempdir_in};
use tracing::{info, warn};

use crate::config::{self};
use crate::sandbox;
use crate::types::UserInterface;

// TODO: Remove this after a while.
const LEGACY_CONFIG_DIR: &str = ".ethersync";

pub fn setup_teamtype_directory(
    directory: Option<&Path>,
    ui: &UserInterface,
) -> Result<(PathBuf, Option<TempDir>)> {
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
    if has_ethersync_directory(&base_dir) {
        let old_directory = base_dir.join(LEGACY_CONFIG_DIR);

        warn!(
            "You have an '{}/' directory, back from when the project was called \"Ethersync\" until October 2025.",
            &old_directory.display()
        );

        if ui.confirm(&format!(
            "Do you want to rename {}/ to {}/?",
            LEGACY_CONFIG_DIR,
            config::CONFIG_DIR,
        ))? {
            let new_directory = base_dir.join(config::CONFIG_DIR);
            sandbox::rename_file(&base_dir, &old_directory, &new_directory)?;
        } else {
            bail!(
                "Aborting launch. Rename or remove the {LEGACY_CONFIG_DIR} directory yourself to continue."
            );
        }
    }
    if !has_teamtype_directory(&base_dir) {
        let teamtype_dir = base_dir.join(config::CONFIG_DIR);
        if directory.is_none() {
            ui.inform(&format!(
                "'{}' is the temporary directory that is used as a Teamtype directory.",
                &base_dir.display()
            ));
            sandbox::create_dir(&base_dir, &teamtype_dir)?;
        } else {
            ui.inform(&format!(
                "'{}' hasn't been used as a Teamtype directory before.",
                base_dir.display(),
            ));
            if ui.confirm(&format!(
                "Do you want to enable live collaboration here? (This will create an {}/ directory.)",
                config::CONFIG_DIR
            ))? {
                sandbox::create_dir(&base_dir, &teamtype_dir)?;
                info!("Created! Resuming launch.");
            } else {
                bail!("Aborting launch. Teamtype needs a .teamtype/ directory to function");
            }
        }
    }
    Ok((base_dir, temp_dir))
}

fn has_ethersync_directory(dir: &Path) -> bool {
    let ethersync_dir = dir.join(LEGACY_CONFIG_DIR);
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
