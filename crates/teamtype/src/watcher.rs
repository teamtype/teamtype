// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::future::pending;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

use futures::executor;
use tokio::{
    sync::mpsc::{self, Receiver, Sender},
    time::sleep,
};
use tracing::debug;

use crate::config::{ProjectDir, VcsMode};
use crate::sandbox;

#[derive(Debug, PartialEq, Eq)]
pub struct WatcherEvent {
    pub file_path: PathBuf,
    pub event_type: WatcherEventType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WatcherEventType {
    Created,
    Removed,
    Changed,
}

struct PendingEvent {
    event_type: WatcherEventType,
    timestamp: SystemTime,
}

#[cfg(test)]
mod tests {
    use tempfile::tempdir;

    use super::*;
    use crate::config::Config;

    fn create_temp_dir_and_config() -> (PathBuf, Config) {
        let dir = tempdir().expect("Failed to create temp directory");

        // We canonicalize the path here, because on macOS, TempDir gives us paths in /var/, which
        // symlinks to /private/var/. But the paths in the file events are always in /private/var/.
        // If we wouldn't canonicalize, the watcher would ignore basically all events.
        let dir_path = dir.path().canonicalize().unwrap();

        let config = Config {
            project_dir: ProjectDir::Temporary(dir),
            ..Default::default()
        };

        (dir_path, config)
    }

    #[tokio::test]
    async fn create() {
        let (dir_path, config) = create_temp_dir_and_config();

        let mut file = dir_path.clone();
        file.push("file");

        let mut watcher = Watcher::spawn(config.project_dir, config.vcs_mode);
        sandbox::write_file(&dir_path, &file, b"hi").unwrap();

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file,
                event_type: WatcherEventType::Created,
            })
        );
    }

    #[tokio::test]
    #[cfg(target_os = "linux")]
    async fn change() {
        let (dir_path, config) = create_temp_dir_and_config();

        let mut file = dir_path.clone();
        file.push("file");
        sandbox::write_file(&dir_path, &file, b"hi").unwrap();

        let mut watcher = Watcher::spawn(config.project_dir, config.vcs_mode);

        sandbox::write_file(&dir_path, &file, b"yo").unwrap();

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file,
                event_type: WatcherEventType::Changed,
            })
        );
    }

    #[tokio::test]
    async fn remove() {
        let (dir_path, config) = create_temp_dir_and_config();

        let mut file = dir_path.clone();
        file.push("file");
        sandbox::write_file(&dir_path, &file, b"hi").unwrap();

        let mut watcher = Watcher::spawn(config.project_dir, config.vcs_mode);

        sandbox::remove_file(&dir_path, &file).unwrap();

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file,
                event_type: WatcherEventType::Removed,
            })
        );
    }

    #[tokio::test]
    async fn rename() {
        let (dir_path, config) = create_temp_dir_and_config();

        let mut file = dir_path.clone();
        file.push("file");
        let mut file_new = dir_path.clone();
        file_new.push("file2");
        sandbox::write_file(&dir_path, &file, b"hi").unwrap();

        let mut watcher = Watcher::spawn(config.project_dir, config.vcs_mode);

        sandbox::rename_file(&dir_path, &file, &file_new).unwrap();

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file,
                event_type: WatcherEventType::Removed,
            })
        );

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file_new,
                event_type: WatcherEventType::Created,
            })
        );
    }

    #[tokio::test]
    async fn ignore() {
        let (dir_path, config) = create_temp_dir_and_config();

        let mut gitignore = dir_path.clone();
        gitignore.push(".ignore");
        sandbox::write_file(&dir_path, &gitignore, b"file").unwrap();

        sleep(Duration::from_millis(100)).await;
        let mut watcher = Watcher::spawn(config.project_dir, config.vcs_mode);

        let mut file = dir_path.clone();
        file.push("file");
        let mut file2 = dir_path.clone();
        file2.push("file2");

        sandbox::write_file(&dir_path, &file, b"hi").unwrap();
        sandbox::write_file(&dir_path, &file2, b"ho").unwrap();

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file2,
                event_type: WatcherEventType::Created,
            })
        );
    }

    #[tokio::test]
    async fn remove_then_create() {
        let (dir_path, config) = create_temp_dir_and_config();

        let mut file = dir_path.clone();
        file.push("file");
        sandbox::write_file(&dir_path, &file, b"hi").unwrap();

        let mut watcher = Watcher::spawn(config.project_dir, config.vcs_mode);

        sandbox::remove_file(&dir_path, &file).unwrap();
        sandbox::write_file(&dir_path, &file, b"i'm back").unwrap();

        assert_eq!(
            watcher.recv().await,
            Some(WatcherEvent {
                file_path: file,
                event_type: WatcherEventType::Changed,
            })
        );
    }
}
