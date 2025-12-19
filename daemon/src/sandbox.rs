// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 axelmartensson <axel.martensson@hotmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! 👮🚨🚓
//! The functions in this module are supposed to prevent file I/O outside the base directory.
//! All our file I/O should go through them.

pub mod sandbox_unix;

use std::fmt::Debug;
use crate::config::AppConfig;
use anyhow::{Context, Result, bail};
use ignore::WalkBuilder;
use ignore::overrides::OverrideBuilder;
use path_clean::PathClean;
use std::fs::{self, OpenOptions};
use std::io;
use std::io::Write;
use std::path::{Path, PathBuf};
use dyn_clone::DynClone;
use crate::sandbox::sandbox_unix::UnixSandbox;

pub trait Sandbox: DynClone + Debug + Send + Sync {

    fn read_file(&self, absolute_base_dir: &Path, absolute_file_path: &Path) -> Result<Vec<u8>> {
        let canonical_file_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path)?;
        let bytes = fs::read(canonical_file_path)?;
        Ok(bytes)
    }

    /// Writes content to a file, creating the parent directories, if they don't exist.
    fn write_file(
        &self,
        absolute_base_dir: &Path,
        absolute_file_path: &Path,
        content: &[u8],
    ) -> Result<()> {
        let canonical_file_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path)?;

        // Create the parent directorie(s), if neccessary.
        let parent_dir = canonical_file_path
            .parent()
            .expect("Failed to get parent directory");
        self.create_dir_all(absolute_base_dir, parent_dir).expect("Failed to create parent directory");

        fs::write(canonical_file_path, content)?;
        Ok(())
    }

    fn append_file(
        &self,
        absolute_base_dir: &Path,
        absolute_file_path: &Path,
        content: &[u8],
    ) -> Result<()> {
        let canonical_file_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path)?;
        let mut file = OpenOptions::new().append(true).open(canonical_file_path)?;
        file.write_all(content)?;
        Ok(())
    }

    fn rename_file(
        &self,
        absolute_base_dir: &Path,
        absolute_file_path_old: &Path,
        absolute_file_path_new: &Path,
    ) -> Result<()> {
        let canonical_file_path_old =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path_old)?;
        let canonical_file_path_new =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path_new)?;
        fs::rename(canonical_file_path_old, canonical_file_path_new)?;
        Ok(())
    }

    fn remove_file(&self, absolute_base_dir: &Path, absolute_file_path: &Path) -> Result<()> {
        let canonical_file_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path)?;
        fs::remove_file(canonical_file_path)?;
        Ok(())
    }

    fn create_dir(&self, absolute_base_dir: &Path, absolute_dir_path: &Path) -> Result<()> {
        let canonical_dir_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_dir_path)?;
        let has_dir = canonical_dir_path.exists() && canonical_dir_path.is_dir();
        if !has_dir {
            fs::create_dir(&canonical_dir_path)?;
            self.set_permissions(canonical_dir_path, 0o700)?;
        }
        Ok(())
    }

    fn set_permissions(&self, canonical_dir_path: PathBuf, mode: u32) -> io::Result<()>;

    fn create_dir_all(&self, absolute_base_dir: &Path, absolute_dir_path: &Path) -> Result<()> {
        let canonical_dir_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_dir_path)?;
        fs::create_dir_all(canonical_dir_path)?;
        Ok(())
    }

    fn exists(&self, absolute_base_dir: &Path, absolute_file_path: &Path) -> Result<bool> {
        let canonical_file_path =
            check_inside_base_dir_and_canonicalize(absolute_base_dir, absolute_file_path)?;
        Ok(canonical_file_path.exists())
    }

    fn enumerate_non_ignored_files(&self, app_config: &AppConfig) -> Vec<PathBuf> {
        let mut ignored_things = vec![".teamtype"];
        if !app_config.sync_vcs {
            ignored_things.extend([".teamtype", ".git", ".bzr", ".hg", ".jj", ".pijul", ".svn"]);
        }

        let walk = WalkBuilder::new(&app_config.base_dir)
            .standard_filters(true)
            .hidden(false)
            .require_git(false)
            // Interestingly, the standard filters don't seem to ignore .git.
            .filter_entry(move |dir_entry| {
                let name = dir_entry
                    .path()
                    .file_name()
                    .expect("Failed to get file name from path.")
                    .to_str()
                    .expect("Failed to convert OsStr to str");
                !ignored_things.contains(&name) && !name.ends_with('~')
            })
            .build();

        let mut files: Vec<PathBuf> = walk
            .filter_map(Result::ok)
            .filter(|dir_entry| {
                !dir_entry
                    .file_type()
                    .expect("Couldn't get file type of dir entry")
                    .is_dir()
            })
            .map(|dir_entry| dir_entry.path().to_path_buf())
            .collect();

        // When jj is used colocatedly with Git, there will be a .jj/.gitignore that ignores .jj.
        // With --sync-vcs, we want to un-ignore it, though! The ignore crate's override functionality
        // doesn't seem to allow for that - as soon as we positive-list something, everything else gets
        // ignored.
        // So do a second walk, and merge the results.
        if app_config.sync_vcs {
            let overrides = OverrideBuilder::new(app_config.base_dir.clone())
                .add(".jj/")
                .expect("Failed to add pattern to OverrideBuilder")
                .add(".jj/**")
                .expect("Failed to add pattern to OverrideBuilder")
                .build()
                .expect("Failed to build Overrides");
            let walk = WalkBuilder::new(&app_config.base_dir)
                .overrides(overrides)
                .build();
            let jj_files: Vec<PathBuf> = walk
                .filter_map(Result::ok)
                .filter(|dir_entry| {
                    !dir_entry
                        .file_type()
                        .expect("Couldn't get file type of dir entry")
                        .is_dir()
                })
                .map(|dir_entry| dir_entry.path().to_path_buf())
                .collect();

            // TODO: Remove duplictes, in the case that jj is used non-colocatedly.
            files.extend(jj_files);
        }

        files
    }

    // TODO: Don't build the list of ignored files on every call.
    // TODO: Allow calling this for non-existing files.
    fn ignored(&self, app_config: &AppConfig, absolute_file_path: &Path) -> Result<bool> {
        let canonical_file_path =
            check_inside_base_dir_and_canonicalize(&app_config.base_dir, absolute_file_path)?;

        Ok(!self.enumerate_non_ignored_files(app_config)
            .into_iter()
            .map(|path_buf| absolute_and_canonicalized(&path_buf))
            .collect::<Result<Vec<_>>>()?
            .contains(&canonical_file_path))
    }
}

dyn_clone::clone_trait_object!(Sandbox);


pub fn new() -> Box<dyn Sandbox> {
    Box::new(UnixSandbox{})
}

fn check_inside_base_dir_and_canonicalize(base_dir: &Path, path: &Path) -> Result<PathBuf> {
    let canonical_base_dir = absolute_and_canonicalized(base_dir)?;
    let canonical_path = absolute_and_canonicalized(path)?;

    if !canonical_path.starts_with(&canonical_base_dir) {
        let canonical_path_str = &canonical_path.display();
        let canonical_base_dir_str = &canonical_base_dir.display();
        bail!(
            "File path {canonical_path_str} is not inside the base directory {canonical_base_dir_str}"
        );
    }

    Ok(canonical_path)
}

fn absolute_and_canonicalized(path: &Path) -> Result<PathBuf> {
    if !path.is_absolute() {
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
        if let std::path::Component::Normal(os_str) = component {
            suffix_path = if suffix_path.components().count() != 0 {
                Path::new(os_str).join(&suffix_path)
            } else {
                Path::new(os_str).to_path_buf()
            };
        } else {
            panic!("Got unexpected Component variant while canonicalizing");
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

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::{TempDir, tempdir};
    use crate::sandbox::sandbox_unix::UnixSandbox;

    fn temp_dir_setup() -> TempDir {
        let dir = tempdir().expect("Failed to create temp directory");
        let project_dir = dir.path().join("project");
        fs::create_dir(&project_dir).expect("Failed to create directory");
        fs::write(project_dir.join("a"), b"This is a file").expect("Failed to write file");
        fs::create_dir(project_dir.join("dir")).expect("Failed to create directory");
        fs::write(project_dir.join("dir").join("b"), b"This is b file")
            .expect("Failed to write file");
        fs::write(dir.path().join("secret"), b"This is a secret").expect("Failed to write file");

        dir
    }

    #[test]
    fn does_canonicalize_symlink_dir() {
        let dir = temp_dir_setup();
        let linked_project = dir.path().join("ln_project");
        let project = dir.path().join("project");
        std::os::unix::fs::symlink(&project, &linked_project).unwrap();
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
        std::os::unix::fs::symlink(&project, &linked_project).unwrap();

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
        std::os::unix::fs::symlink(&project, &linked_project).unwrap();

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
        let sandbox = UnixSandbox{};

        assert!(sandbox.read_file(&project_dir, &project_dir.join("a")).is_ok());
        assert!(sandbox.read_file(&project_dir, &project_dir.join("dir").join("b")).is_ok());
        assert!(sandbox.read_file(&project_dir, &project_dir.join("dir").join("..").join("a")).is_ok());
        assert!(
            sandbox.read_file(
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
        let sandbox = UnixSandbox{};

        // Not a file.
        assert!(sandbox.read_file(&project_dir, &project_dir).is_err());

        // Not a file *and* now within base dir.
        assert!(sandbox.read_file(&project_dir, &project_dir.join("..")).is_err());

        // Definitely not within base dir.
        assert!(sandbox.read_file(&project_dir, Path::new("/etc/passwd")).is_err());

        // File path is not absolute.
        assert!(sandbox.read_file(&project_dir, Path::new("project/a")).is_err());

        // Base dir is not absolute.
        assert!(sandbox.read_file(Path::new("project"), &project_dir.join("a")).is_err());

        // File not exist.
        assert!(sandbox.read_file(&project_dir, &project_dir.join("nonexistant")).is_err());
    }

    #[test]
    fn fail_check_inside_base_dir() {
        let dir = temp_dir_setup();
        let project_dir = dir.path().join("project");
        let sandbox = UnixSandbox{};

        // Not within the base dir.
        assert!(sandbox.read_file(&project_dir, &project_dir.join("..").join("secret")).is_err());

        // It "starts" with the base dir, but it's not inside it.
        assert!(
            check_inside_base_dir_and_canonicalize(
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
