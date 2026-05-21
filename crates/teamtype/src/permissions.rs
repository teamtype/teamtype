// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
// SPDX-FileCopyrightText: 2026 dommi <dommihd@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(unix)]
use std::fs::Permissions;
use std::fs::{File, OpenOptions};
#[cfg(unix)]
use std::fs::{metadata, set_permissions};
#[cfg(unix)]
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::path::{Path, PathBuf};

use anyhow::Result;
#[cfg(unix)]
use anyhow::{Context, bail};

pub(crate) fn set_mode(path: PathBuf, mode: u32) -> Result<()> {
    #[cfg(unix)]
    {
        let permissions = Permissions::from_mode(mode);
        set_permissions(path, permissions).context("Failed to mode on file")
    }

    #[cfg(windows)]
    {
        // Windows doesn't have a direct equivalent of Unix permissions, just discard the details
        // of the question and say we're good.
        let _ = path;
        let _ = mode;
        Ok(())
    }
}

pub(crate) fn check_mode(path: &Path, allowed_permissions: u32) -> Result<()> {
    #[cfg(unix)]
    {
        // Only on unix the permissions are checked, because we can't check them on Windows
        let current_permissions = metadata(path)
            .with_context(|| {
                format!(
                    "Failed to read metadata of the directory: {}",
                    &path.display()
                )
            })?
            .permissions()
            .mode();
        if current_permissions & !allowed_permissions != 0 {
            bail!(
                "The current permissions {} did not match the allowed permissions {} for path {}",
                current_permissions,
                allowed_permissions,
                path.display()
            );
        }
    }
    #[cfg(windows)]
    {
        let _ = path;
        let _ = allowed_permissions;
    }
    Ok(())
}

pub(crate) fn create_with_mode(file: PathBuf, mode: u32) -> Result<File> {
    #[cfg(unix)]
    {
        Ok(OpenOptions::new()
            .create_new(true)
            .write(true)
            .mode(mode)
            .open(file)?)
    }
    #[cfg(windows)]
    {
        let _ = mode;
        Ok(OpenOptions::new().create_new(true).write(true).open(file)?)
    }
}
