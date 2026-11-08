// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
// SPDX-FileCopyrightText: 2026 dommi <dommihd@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! 👮🚨🚓
//! The functions in this module are supposed to prevent file I/O outside the project directory.
//! All our file I/O should go through them.

use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::{Component, Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use path_clean::PathClean;

use crate::config::CONFIG_DIR;
use crate::config::{ProjectDir, VcsMode};
use crate::permissions::{create_private_dir, ensure_private_dir};

pub(crate) fn read_file(absolute_project_dir: &Path, absolute_file_path: &Path) -> Result<Vec<u8>> {
    let canonical_file_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path)?;
    let bytes = fs::read(canonical_file_path)?;
    Ok(bytes)
}

/// Writes content to a file, creating the parent directories, if they don't exist.
pub fn write_file(
    absolute_project_dir: &Path,
    absolute_file_path: &Path,
    content: &[u8],
) -> Result<()> {
    let canonical_file_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path)?;

    // Create the parent directory(s), if necessary.
    let parent_dir = canonical_file_path
        .parent()
        .context("Failed to get parent directory")?;
    create_dir_all(absolute_project_dir, parent_dir)
        .context("Failed to create parent directory")?;

    fs::write(canonical_file_path, content)?;
    Ok(())
}

pub(crate) fn append_file(
    absolute_project_dir: &Path,
    absolute_file_path: &Path,
    content: &[u8],
) -> Result<()> {
    let canonical_file_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path)?;
    let mut file = OpenOptions::new().append(true).open(canonical_file_path)?;
    file.write_all(content)?;
    Ok(())
}

pub fn rename_file(
    absolute_project_dir: &Path,
    absolute_file_path_old: &Path,
    absolute_file_path_new: &Path,
) -> Result<()> {
    let canonical_file_path_old =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path_old)?;
    let canonical_file_path_new =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path_new)?;
    fs::rename(canonical_file_path_old, canonical_file_path_new)?;
    Ok(())
}

pub fn remove_file(absolute_project_dir: &Path, absolute_file_path: &Path) -> Result<()> {
    let canonical_file_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path)?;
    fs::remove_file(canonical_file_path)?;
    Ok(())
}

pub fn create_dir(absolute_project_dir: &Path, absolute_dir_path: &Path) -> Result<()> {
    let canonical_dir_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_dir_path)?;
    if !canonical_dir_path.exists() {
        fs::create_dir(&canonical_dir_path)
            .context("Unable to create directory with FS function")?;
    }
    Ok(())
}

/// Create a directory with safe attributes from the outset, or confirm that such already exists.
///
/// Creating a directory or file and then later changing the permissions later is technically not as
/// safe as it sounds. It is possible for a malicious watcher to intercept the filesystem creation
/// event and use that info to open a file handle before unblocking. Then when the process continues
/// and changes the permissions, the existing file open handle will persist even if it no longer has
/// permissions. This loophole only lasts as long as the malicious watching process keeps the handle
/// open, but that's still a security hole we can plug by creating directories and files with the
/// private permission bits set in the kernel umask from the outset.
///
/// Always use this to create directories we want to be considered secure.
pub fn create_dir_with_privacy(
    absolute_project_dir: &Path,
    absolute_dir_path: &Path,
) -> Result<()> {
    let canonical_dir_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_dir_path)?;
    if canonical_dir_path.exists() {
        ensure_private_dir(&canonical_dir_path).context("Safety check on directory failed")?;
    } else {
        create_private_dir(&canonical_dir_path).context("Unable to safely create directory")?;
    }
    Ok(())
}

pub(crate) fn create_dir_all(absolute_project_dir: &Path, absolute_dir_path: &Path) -> Result<()> {
    let canonical_dir_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_dir_path)?;
    if !canonical_dir_path.exists() {
        fs::create_dir_all(canonical_dir_path)
            .context("Unable to create directory(s) with FS function")?;
    }
    Ok(())
}

pub fn exists(absolute_project_dir: &Path, absolute_file_path: &Path) -> Result<bool> {
    let canonical_file_path =
        check_inside_project_dir_and_canonicalize(absolute_project_dir, absolute_file_path)?;
    Ok(canonical_file_path.exists())
}

fn check_inside_project_dir_and_canonicalize(project_dir: &Path, path: &Path) -> Result<PathBuf> {
    let canonical_project_dir = absolute_and_canonicalized(project_dir)?;
    let canonical_path = absolute_and_canonicalized(path)?;

    if !canonical_path.starts_with(&canonical_project_dir) {
        let canonical_path_str = &canonical_path.display();
        let canonical_project_dir_str = &canonical_project_dir.display();
        bail!(
            "File path {canonical_path_str} is not inside the project directory {canonical_project_dir_str}"
        );
    }

    Ok(canonical_path)
}

pub(crate) fn absolute_and_canonicalized(path: &Path) -> Result<PathBuf> {
    if !path.has_root() {
        bail!("Path is not absolute.");
    }

    // Remove any ".." and "." from the path.
    let canonical_path = path.clean();
    let mut suffix_path = PathBuf::new();
    let mut prefix_path = canonical_path;

    for component in path.components().rev() {
        if prefix_path.exists() {
            break;
        }
        prefix_path.pop();
        if let Component::Normal(os_str) = component {
            suffix_path = if suffix_path.components().count() != 0 {
                Path::new(os_str).join(&suffix_path)
            } else {
                Path::new(os_str).to_path_buf()
            };
        } else {
            bail!("Got unexpected Component variant while canonicalizing");
        }
    }

    let mut canonical_path = prefix_path
        .canonicalize()
        .context("Failed to canonicalize path, probably the file disappeared already")?;

    if suffix_path.components().count() != 0 {
        canonical_path = canonical_path.join(suffix_path);
    }

    Ok(canonical_path)
}

#[cfg(unix)]
#[cfg(test)]
mod tests {
    use std::os::unix::fs::symlink;

    use tempfile::{TempDir, tempdir};

    use super::*;

    fn temp_dir_setup() -> TempDir {
        let dir = tempdir().expect("Failed to create temp directory");
        let project_dir = dir.path().join("project");
        fs::create_dir(&project_dir).expect("Failed to create directory");
        fs::write(project_dir.join("a"), b"This is a file").expect("Failed to write file");
        fs::create_dir(project_dir.join("dir")).expect("Failed to create directory");
        fs::write(project_dir.join("dir").join("b"), b"This is b file")
            .expect("Failed to write file");
        fs::write(
            dir.path().join("file-outside-of-project"),
            b"This is a file outside of project",
        )
        .expect("Failed to write file");

        dir
    }

    #[test]
    fn does_canonicalize_symlink_dir() {
        let dir = temp_dir_setup();
        let linked_project = dir.path().join("ln_project");
        let project = dir.path().join("project");
        symlink(&project, &linked_project).unwrap();
        assert_eq!(
            absolute_and_canonicalized(&linked_project).unwrap(),
            project.canonicalize().unwrap()
        );
    }

    #[test]
    fn does_canonicalize_symlink_file() {
        let dir = temp_dir_setup();
        let linked_project = dir.path().join("ln_project");
        let project = dir.path().join("project");
        symlink(&project, &linked_project).unwrap();

        let ln_file = linked_project.join("c");

        assert_eq!(
            absolute_and_canonicalized(&ln_file).unwrap().to_str(),
            project.canonicalize().unwrap().join("c").to_str()
        );
    }

    #[test]
    fn does_canonicalize_symlink_notexisting_file() {
        let dir = temp_dir_setup();
        let linked_project = dir.path().join("ln_project");
        let project = dir.path().join("project");
        symlink(&project, &linked_project).unwrap();

        let file = project.join("a");
        let ln_file = linked_project.join("a");

        // tests whether it does not end on slash
        assert_eq!(
            absolute_and_canonicalized(&file).unwrap().to_str(),
            file.canonicalize().unwrap().to_str()
        );

        assert_eq!(
            absolute_and_canonicalized(&ln_file).unwrap().to_str(),
            ln_file.canonicalize().unwrap().to_str()
        );
    }

    #[test]
    fn can_read_in_dir() {
        let dir = temp_dir_setup();
        let project_dir = dir.path().join("project");

        assert!(read_file(&project_dir, &project_dir.join("a")).is_ok());
        assert!(read_file(&project_dir, &project_dir.join("dir").join("b")).is_ok());
        assert!(read_file(&project_dir, &project_dir.join("dir").join("..").join("a")).is_ok());
        assert!(
            read_file(
                &project_dir,
                &project_dir.join(".").join("dir").join(".").join("b")
            )
            .is_ok()
        );
    }

    #[test]
    fn can_not_read_outside_dir() {
        let dir = temp_dir_setup();
        let project_dir = dir.path().join("project");

        // Not a file.
        assert!(read_file(&project_dir, &project_dir).is_err());

        // Not a file *and* now within project dir.
        assert!(read_file(&project_dir, &project_dir.join("..")).is_err());

        // Definitely not within project dir.
        assert!(read_file(&project_dir, Path::new("/etc/passwd")).is_err());

        // File path is not absolute.
        assert!(read_file(&project_dir, Path::new("project/a")).is_err());

        // Project dir is not absolute.
        assert!(read_file(Path::new("project"), &project_dir.join("a")).is_err());

        // File not exist.
        assert!(read_file(&project_dir, &project_dir.join("nonexistent")).is_err());
    }

    #[test]
    fn obeys_teamtypeignore() {
        let dir = temp_dir_setup();
        let project_dir = dir.path().join("project");
        let teamtypeignore = project_dir.join(".teamtypeignore");

        fs::write(&teamtypeignore, b"a\n").expect("Failed to write .teamtypeignore");

        let project_dir = ProjectDir::Permanent(project_dir);

        assert!(
            ignored(&project_dir, VcsMode::Ignore, &project_dir.join("a")).unwrap(),
            "a should be ignored"
        );
        assert!(
            !ignored(&project_dir, VcsMode::Ignore, &project_dir.join("dir/b")).unwrap(),
            "b should be not ignored"
        );

        fs::write(&teamtypeignore, b"a\ndir\n").expect("Failed to write .teamtypeignore");

        assert!(
            ignored(&project_dir, VcsMode::Ignore, &project_dir.join("dir/b")).unwrap(),
            "now b should be ignored"
        );
    }

    #[test]
    fn fail_check_inside_project_dir() {
        let dir = temp_dir_setup();
        let project_dir = dir.path().join("project");

        // Not within the project dir.
        assert!(
            read_file(
                &project_dir,
                &project_dir.join("..").join("file-outside-of-project")
            )
            .is_err()
        );

        // It "starts" with the project dir, but it's not inside it.
        assert!(
            check_inside_project_dir_and_canonicalize(
                &project_dir,
                Path::new(&format!(
                    "{}{}",
                    project_dir.as_path().to_str().unwrap(),
                    "2/file"
                ))
            )
            .is_err()
        );
    }
}
