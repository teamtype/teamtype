// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::{self, Path, PathBuf};

use anyhow::Context;
use anyhow::bail;
use automerge::Prop;
use derive_more::{AsRef, Deref, Display};
use serde::{Deserialize, Serialize};
use url::Url;

/// Paths like these are guaranteed to be absolute.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Eq, Hash, Deref, AsRef, Display)]
#[as_ref(Path)]
#[display("'{}'", self.0.display())]
#[must_use]
pub struct AbsolutePath(PathBuf);

impl AbsolutePath {
    pub fn from_parts(base: &Path, relative_path: &RelativePath) -> Result<Self, anyhow::Error> {
        let path = base.join(relative_path);
        Self::try_from(path)
    }

    pub fn to_file_uri(&self) -> FileUri {
        // Figure out how to prefix paths to make them file URLs. On *nix this is simple. On Windows
        // it is a three ring circus. Not my circus not my monkeys.
        let uri = match Url::from_file_path(&self.0) {
            // If the url crate likes the path, we don't need any monkey business.
            Ok(url) => url.to_string(),
            // On the other hand if the url crate thinks the path is nonsense, it probably is.
            // Windows has several wonkey options it gives back when we canonicalize() relative to
            // the project. There are multiple possible absolute prefixes we need to normalize.
            Err(()) => pseudo_absolute_path_to_file_uri(&self.0),
        };
        // Percent-encode the file path to be valid as a URI.
        FileUri::try_from(uri).expect("Should be able to create File URI from absolute path")
    }
}

fn pseudo_absolute_path_to_file_uri(path: &Path) -> String {
    let path = path.to_string_lossy();
    // On Windows, `canonicalize()` prefixes results with a verbatim `\\?\` marker. This doesn't
    // help determine which 'root' drive to start from, so strip it. We should be left with a drive
    // letter or UNC drive form.
    let path = path.trim_start_matches(r"\\?\");
    let path = if cfg!(windows) {
        path.replace('\\', "/")
    } else {
        path.to_string()
    };
    // At this point if we have a UNC drive form it will start with '/'. Otherwise it will be
    // a drive letter and we should add the pseudo-root morker ourselves.
    let path = if path.starts_with('/') {
        path
    } else {
        format!("/{path}")
    };
    format!("file://{path}")
}

impl TryFrom<PathBuf> for AbsolutePath {
    type Error = anyhow::Error;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        if !path.has_root() {
            bail!("Path '{}' is not absolute", path.display());
        }

        Ok(Self(path))
    }
}

impl TryFrom<&str> for AbsolutePath {
    type Error = anyhow::Error;

    fn try_from(path: &str) -> Result<Self, Self::Error> {
        Path::new(&path).to_path_buf().try_into()
    }
}

/// Paths like these are relative to the shared directory.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Eq, Hash, Deref, AsRef, Display)]
#[as_ref(Path)]
#[display("'{}'", self.0.display())]
#[must_use]
pub struct RelativePath(PathBuf);

impl RelativePath {
    // TODO: This doesn't check the parameter in any way. Should it?
    pub fn new(path: &str) -> Self {
        Self(path.into())
    }

    pub fn try_from_absolute(project_dir: &Path, path: &AbsolutePath) -> Result<Self, anyhow::Error> {
        let shared_dir = path::absolute(project_dir).with_context(|| {
            format!(
                "Failed to get absolute path for shared directory '{}'",
                project_dir.display()
            )
        })?;
        let relative_path = path.strip_prefix(&shared_dir).with_context(|| {
            format!(
                "The path {path} is not in the shared directory '{}'. Your plugin probably doesn't support opening files from multiple Teamtype directories.",
                shared_dir.display()
            )
        })?;

        if relative_path.iter().count() == 0 {
            bail!("project_dir was equal to path when computing relative path");
        }

        Ok(Self(relative_path.to_path_buf()))
    }

    pub fn try_from_path(project_dir: &Path, path: &Path) -> Result<Self, anyhow::Error> {
        let absolute_path = AbsolutePath::try_from(path.to_path_buf())?;
        Self::try_from_absolute(project_dir, &absolute_path)
    }
}

impl From<&RelativePath> for Prop {
    fn from(val: &RelativePath) -> Self {
        Self::Map(val.0.display().to_string())
    }
}

// TODO: Wrap the newtype around url::Url instead?
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deref)]
#[must_use]
pub struct FileUri(String);

impl FileUri {
    pub fn to_absolute_path(&self) -> AbsolutePath {
        let path_buf = Url::parse(self)
            .expect("Should be able to parse file:// URL as Url")
            .to_file_path()
            .expect("Should be able to convert Url to PathBuf");
        AbsolutePath::try_from(path_buf).expect("File URI should contain an absolute path")
    }
}

impl TryFrom<String> for FileUri {
    type Error = anyhow::Error;

    fn try_from(string: String) -> Result<Self, Self::Error> {
        // TODO: Could be written simpler?
        if string.starts_with("file:///") {
            // Use the url crate to properly URL encode the path (spaces should be "%20", for example).
            Ok(Self(
                Url::parse(&string)
                    .expect("Should be able to parse file:// URL")
                    .to_string(),
            ))
        } else {
            bail!("File URI '{string}' does not start with 'file:///'");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_file_path_for_uri_fails_not_absolute() {
        assert!(AbsolutePath::try_from("this/is/absolutely/not/absolute").is_err());
    }

    #[test]
    fn test_file_path_for_uri_fails_not_within_project_dir() {
        let project_dir = Path::new("/an/absolute/path");
        let path = AbsolutePath::try_from("/a/very/different/path").unwrap();

        assert!(RelativePath::try_from_absolute(project_dir, &path,).is_err());
    }

    #[test]
    fn test_file_path_for_uri_fails_not_within_project_dir_suffix() {
        let project_dir = Path::new("/an/absolute/path");
        let path = AbsolutePath::try_from("/an/absolute/path2/file").unwrap();

        assert!(RelativePath::try_from_absolute(project_dir, &path,).is_err());
    }

    #[test]
    fn test_file_path_for_uri_fails_only_project_dir() {
        let project_dir = Path::new("/an/absolute/path");
        let path = AbsolutePath::try_from("/an/absolute/path").unwrap();

        assert!(RelativePath::try_from_absolute(project_dir, &path,).is_err());
    }

    #[test]
    fn test_file_path_for_uri_works() {
        let project_dir = Path::new("/an/absolute/path");

        let file_paths = vec!["file1", "sub/file3", "sub"];
        for &expected in &file_paths {
            let uri =
                FileUri::try_from(format!("file://{}/{}", project_dir.display(), expected)).unwrap();
            let absolute_path = uri.to_absolute_path();
            let relative_path = RelativePath::try_from_absolute(project_dir, &absolute_path).unwrap();

            assert_eq!(RelativePath::new(expected), relative_path);
        }
    }

    #[test]
    fn test_uri_encoding_works_with_spaces() {
        let uri = FileUri::try_from("file:///a/b/file with spaces".to_string()).unwrap();
        assert_eq!("file:///a/b/file%20with%20spaces", uri.0);
    }

    #[test]
    fn test_uri_decoding_works_with_spaces() {
        let uri = FileUri::try_from("file:///a/b/file with spaces".to_string()).unwrap();
        let absolute_path = AbsolutePath::try_from("/a/b/file with spaces").unwrap();
        assert_eq!(absolute_path, uri.to_absolute_path());
    }
}
