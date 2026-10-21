// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
// SPDX-FileCopyrightText: 2026 dommi <dommihd@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fs::symlink_metadata;
use std::fs::{DirBuilder, File, Metadata, OpenOptions};
#[cfg(unix)]
use std::os::unix::fs::{DirBuilderExt, OpenOptionsExt, PermissionsExt};
#[cfg(windows)]
use std::os::windows::fs::{MetadataExt, OpenOptionsExt};
use std::path::{Path, PathBuf};

use anyhow::ensure;
use anyhow::{Context, Result};
use tracing::debug;

#[cfg(unix)]
const MODE_DIR_PRIVATE: u32 = 0o700;
#[cfg(unix)]
const MODE_FILE_PRIVATE: u32 = 0o600;
#[cfg(windows)]
const ATTRIBUTE_HIDDEN: u32 = 0x2;

/// Create a directory with a reasonable expectation of privacy. Depending on the platform this may
/// or may not be an effective security barrier. On Unix it should only be readable by the creating
/// user (or root). On Windows it will just be hidden from normal file browsing.
pub fn create_private_dir(path: &Path) -> Result<()> {
    let mut builder = DirBuilder::new();
    builder.recursive(false);

    #[cfg(unix)]
    builder.mode(MODE_DIR_PRIVATE);

    // TODO: When https://github.com/rust-lang/rust/issues/152956 lands in stable and
    // set_permissions() based on SetFileAttributes is no longer gated as a nightly feature, we can
    // set this attribute on directories as well as files.
    // This is a decluttering convenience not a security guard.
    // #[cfg(windows)]
    // builder.attributes(ATTRIBUTE_HIDDEN);

    Ok(builder.create(path)?)
}

/// Create a file (and return a writable file handle) with a reasonable expectation of privacy.
/// Depending on the platform this may or may not be an effective security barrier. On Unix it
/// should be readable and writable only by the creating user (or root). On Windows it will just be
/// hidden from normal file browsing.
pub fn create_private_file(file: PathBuf) -> Result<File> {
    let mut options = OpenOptions::new();
    options.create_new(true).write(true);

    #[cfg(unix)]
    options.mode(MODE_FILE_PRIVATE);

    #[cfg(windows)]
    options.attributes(ATTRIBUTE_HIDDEN);

    Ok(options.open(file)?)
}

/// Verify that an existing directory was created with a reasonable expectation of privacy.
pub fn ensure_private_dir(path: &Path) -> Result<()> {
    let meta = symlink_metadata(path)?;

    ensure!(
        meta.file_type().is_dir(),
        "Path is not a directory, using a symlink is not safe.",
    );

    #[cfg(unix)]
    ensure_no_group_or_world_permissions(&meta)
        .with_context(|| format!("Disallowed permissions found on {}", path.display()))?;

    // TODO: See note above about applying ATTRIBUTE_HIDDEN to directories.
    // #[cfg(windows)]
    // ensure_hidden(&meta)
    //     .with_context(|| format!("Disallowed attributes found on {}", path.display()))?;

    Ok(())
}

/// Verify that an existing file was created with a reasonable expectation of privacy.
pub fn ensure_private_file(path: &Path) -> Result<()> {
    let meta = symlink_metadata(path)?;

    ensure!(
        meta.file_type().is_file(),
        "Path is not a file, using a symlink is not safe.",
    );

    #[cfg(unix)]
    ensure_no_group_or_world_permissions(&meta)
        .with_context(|| format!("Disallowed permissions found on {}", path.display()))?;

    #[cfg(windows)]
    ensure_hidden(&meta)
        .with_context(|| format!("Disallowed attributes found on {}", path.display()))?;

    Ok(())
}

#[cfg(unix)]
fn ensure_no_group_or_world_permissions(meta: &Metadata) -> Result<()> {
    let actual_mode = meta.permissions().mode();
    debug!("Since Unix, checking mode: '0o{actual_mode:o}'");
    let private_mask: u32 = 0o0077;
    ensure!(
        actual_mode & private_mask == 0,
        "Path is readable, writable, or executable by user group or world."
    );
    Ok(())
}

#[cfg(windows)]
fn ensure_hidden(meta: &Metadata) -> Result<()> {
    let actual_attributes = meta.file_attributes();
    debug!("Since Windows, checking attributes: '0x{actual_attributes:x}'");
    ensure!(
        actual_attributes & ATTRIBUTE_HIDDEN == ATTRIBUTE_HIDDEN,
        "Path is readable, writable, or executable by user group or world."
    );
    Ok(())
}
