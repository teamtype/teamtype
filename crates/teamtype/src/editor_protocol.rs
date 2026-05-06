// SPDX-FileCopyrightText: 2025 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2025 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::bail;
use serde::{Deserialize, Serialize};

use crate::types::{CursorId, EditorTextDelta, Range};

type DocumentUri = String;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum OutgoingMessage {
    Notification(EditorProtocolMessageToEditor),
    Response(JSONRPCResponse),
}

impl OutgoingMessage {
    pub fn to_jsonrpc(&self) -> Result<String, anyhow::Error> {
        let json_value =
            serde_json::to_value(self).expect("Failed to convert editor message to a JSON value");
        if let serde_json::Value::Object(mut map) = json_value {
            map.insert("jsonrpc".to_string(), "2.0".into());
            let payload = serde_json::to_string(&map)?;
            Ok(payload)
        } else {
            bail!("EditorProtocolMessage was not serialized to a map");
        }
    }
}

/// An identifier based on the [JSON-RPC spec](https://www.jsonrpc.org/specification):
///
/// > An identifier established by the Client that MUST contain a String, Number, or NULL value if
/// > included. If it is not included it is assumed to be a notification. The value SHOULD normally
/// > not be Null [1](https://www.jsonrpc.org/specification#id1) and Numbers SHOULD NOT contain
/// > fractional parts [2](https://www.jsonrpc.org/specification#id2)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum JSONRPCId {
    Usize(usize),
    String(String),
}

impl From<usize> for JSONRPCId {
    fn from(value: usize) -> Self {
        Self::Usize(value)
    }
}

impl From<String> for JSONRPCId {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IncomingMessage {
    Request {
        id: JSONRPCId,
        #[serde(flatten)]
        payload: EditorProtocolMessageFromEditor,
    },
    Notification {
        #[serde(flatten)]
        payload: EditorProtocolMessageFromEditor,
    },
}
impl IncomingMessage {
    pub fn from_jsonrpc(jsonrpc: &str) -> Result<Self, anyhow::Error> {
        let message = serde_json::from_str(jsonrpc)?;
        Ok(message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum EditorProtocolMessageToEditor {
    Edit {
        uri: DocumentUri,
        revision: usize,
        delta: EditorTextDelta,
    },
    Cursor {
        userid: CursorId,
        name: Option<String>,
        uri: DocumentUri,
        ranges: Vec<Range>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum JSONRPCResponse {
    RequestSuccess {
        id: JSONRPCId,
        result: String,
    },
    RequestError {
        // id must be Null if there was an error detecting the id in the Request Object.
        id: Option<JSONRPCId>,
        error: EditorProtocolMessageError,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EditorProtocolMessageError {
    pub code: i32,
    pub message: String,
    pub data: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum EditorProtocolMessageFromEditor {
    Open {
        uri: DocumentUri,
        content: String,
    },
    Close {
        uri: DocumentUri,
    },
    Edit {
        uri: DocumentUri,
        revision: usize,
        delta: EditorTextDelta,
    },
    Cursor {
        uri: DocumentUri,
        ranges: Vec<Range>,
    },
}

#[cfg(test)]
mod test_serde {

    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn open() {
        let message = IncomingMessage::from_jsonrpc(
            r#"{"jsonrpc":"2.0","id":1,"method":"open","params":{"uri":"file:\/\/\/tmp\/file","content":"initial content"}}"#,
        );
        assert_eq!(
            message.unwrap(),
            IncomingMessage::Request {
                id: 1.into(),
                payload: EditorProtocolMessageFromEditor::Open {
                    uri: "file:///tmp/file".into(),
                    content: "initial content".to_string(),
                }
            }
        );
    }

    #[test]
    fn open_with_string_id() {
        let message = IncomingMessage::from_jsonrpc(
            r#"{"jsonrpc":"2.0","id":"1","method":"open","params":{"uri":"file:\/\/\/tmp\/file","content":"initial content"}}"#,
        );
        assert_eq!(
            message.unwrap(),
            IncomingMessage::Request {
                id: "1".to_string().into(),
                payload: EditorProtocolMessageFromEditor::Open {
                    uri: "file:///tmp/file".into(),
                    content: "initial content".to_string(),
                }
            }
        );
    }

    #[test]
    fn success() {
        let message = OutgoingMessage::Response(JSONRPCResponse::RequestSuccess {
            id: 1.into(),
            result: "success".to_string(),
        });
        let jsonrpc = message.to_jsonrpc();
        assert_eq!(
            jsonrpc.unwrap(),
            r#"{"id":1,"jsonrpc":"2.0","result":"success"}"#
        );
    }

    #[test]
    fn success_with_string_id() {
        let message = OutgoingMessage::Response(JSONRPCResponse::RequestSuccess {
            id: "1".to_string().into(),
            result: "success".to_string(),
        });
        let jsonrpc = message.to_jsonrpc();
        assert_eq!(
            jsonrpc.unwrap(),
            r#"{"id":"1","jsonrpc":"2.0","result":"success"}"#
        );
    }

    #[test]
    fn error() {
        let message = OutgoingMessage::Response(JSONRPCResponse::RequestError {
            id: Some(1.into()),
            error: EditorProtocolMessageError {
                code: -1,
                message: "title".into(),
                data: Some("content".into()),
            },
        });
        let jsonrpc = message.to_jsonrpc();
        assert_eq!(
            jsonrpc.unwrap(),
            r#"{"error":{"code":-1,"data":"content","message":"title"},"id":1,"jsonrpc":"2.0"}"#
        );
    }
}
