// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;

use tracing::debug;

use crate::config::{ProjectDir, VcsMode};
use crate::{
    editor_protocol::{
        EditorProtocolMessageError, EditorProtocolMessageFromEditor, EditorProtocolMessageToEditor,
    },
    ot::OTServer,
    path::{AbsolutePath, FileUri, RelativePath},
    sandbox,
    types::{ComponentMessage, CursorState, RevisionedEditorTextDelta},
};

/// Represents a connection to an editor. Handles the OT. To keep the code testable and sync, we do
/// the actual sending of messages in the daemon, and the functions here just *calculate* them.
#[must_use]
pub struct EditorConnection {
    id: String,
    project_dir: ProjectDir,
    /// There's one [`OTServer`] per open buffer.
    ot_servers: HashMap<RelativePath, OTServer>,
    /// The name other people see.
    username: Option<String>,
    vcs_mode: VcsMode,
}

impl EditorConnection {
    pub fn new(
        id: String,
        project_dir: ProjectDir,
        vcs_mode: VcsMode,
        username: Option<String>,
    ) -> Self {
        Self {
            id,
            project_dir,
            ot_servers: HashMap::new(),
            username,
            vcs_mode,
        }
    }

    #[must_use]
    pub fn owns(&self, file_path: &RelativePath) -> bool {
        self.ot_servers.contains_key(file_path)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use pretty_assertions::assert_eq;
    use tempfile::tempdir;

    use super::*;
    use crate::types::factories::*;

    #[test]
    fn opening_file_in_wrong_dir_fails() {
        let dir = tempdir().expect("Failed to create temp directory");

        let project_dir = ProjectDir::Permanent(dir.path().to_path_buf());
        let username = Some(String::new());

        let mut editor_connection =
            EditorConnection::new("1".to_string(), project_dir, VcsMode::Ignore, username);

        let result =
            editor_connection.message_from_editor(&EditorProtocolMessageFromEditor::Open {
                uri: "file:///foobar/file".to_string(),
                content: String::new(),
            });

        assert!(result.is_err());
    }

    #[test]
    fn edits_are_oted() {
        let dir = tempdir().expect("Failed to create temp directory");
        let file = dir.path().join("file");
        fs::write(&file, "hello").expect("Failed to write file");

        let project_dir = ProjectDir::Permanent(dir.path().to_path_buf());
        let username = Some(String::new());

        let mut editor_connection =
            EditorConnection::new("1".to_string(), project_dir, VcsMode::Ignore, username);

        // Editor opens the file.
        let result =
            editor_connection.message_from_editor(&EditorProtocolMessageFromEditor::Open {
                uri: format!("file://{}", file.display()),
                content: "initial content".to_string(),
            });
        assert_eq!(
            result,
            Ok((
                ComponentMessage::Open {
                    file_path: RelativePath::new("file"),
                    content: "initial content".to_string(),
                },
                vec![]
            ))
        );

        // Daemon sends an edit.
        let delta = insert(1, "x"); // hello -> hxello
        let result = editor_connection.message_from_inside(&ComponentMessage::Edit {
            file_path: RelativePath::new("file"),
            delta,
        });
        assert_eq!(
            result,
            vec![EditorProtocolMessageToEditor::Edit {
                uri: format!("file://{}", file.display()),
                revision: 0,
                delta: ed_delta_single((0, 1), (0, 1), "x")
            }]
        );

        // Editor sends an edit.
        let result =
            editor_connection.message_from_editor(&EditorProtocolMessageFromEditor::Edit {
                uri: format!("file://{}", file.display()),
                revision: 0,
                delta: ed_delta_single((0, 3), (0, 3), "y"),
            });
        let (inside_message, messages_to_editor) = result.unwrap();
        let delta = insert(4, "y"); // Position gets transformed!
        assert_eq!(
            inside_message,
            ComponentMessage::Edit {
                file_path: RelativePath::new("file"),
                delta
            }
        );
        assert_eq!(
            messages_to_editor,
            vec![EditorProtocolMessageToEditor::Edit {
                uri: format!("file://{}", file.display()),
                revision: 1,
                delta: ed_delta_single((0, 1), (0, 1), "x") // Delta is still the
                                                            // same.
            }]
        );
    }
}
