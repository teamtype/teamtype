use anyhow::Result;
use pretty_assertions::assert_eq;
use teamtype::{
    editor_protocol::{EditorProtocolMessageFromEditor, IncomingMessage},
    sandbox,
    types::{EditorTextDelta, EditorTextOp, Position, Range},
};
use teamtype_integration_tests::socket::MockSocket;

async fn expect_request(socket: &mut MockSocket, expected: EditorProtocolMessageFromEditor) {
    loop {
        let msg = socket.recv().await;

        let message: IncomingMessage =
            serde_json::from_str(&msg.to_string()).expect("Could not parse EditorProtocolMessage");

        if let IncomingMessage::Request {
            payload: message, ..
        } = message
        {
            if let EditorProtocolMessageFromEditor::Cursor { .. } = message {
                // Ignore cursor messages.
                continue;
            }
            assert_eq!(expected, message);
        } else {
            panic!("Expected Request, got Notification");
        }
        return;
    }
}

// TODO: Avoid the duplication with expect_request.
async fn expect_notification(socket: &mut MockSocket, expected: EditorProtocolMessageFromEditor) {
    loop {
        let msg = socket.recv().await;

        let message: IncomingMessage =
            serde_json::from_str(&msg.to_string()).expect("Could not parse EditorProtocolMessage");

        if let IncomingMessage::Notification {
            payload: message, ..
        } = message
        {
            assert_eq!(expected, message);
        } else {
            if let IncomingMessage::Request {
                payload: EditorProtocolMessageFromEditor::Cursor { .. },
                ..
            } = message
            {
                // Ignore cursor messages.
                continue;
            }
            panic!("Expected Notification, got Request");
        }
        return;
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let dir = temp_dir::TempDir::new().expect("Failed to create temp directory");
    let mut teamtype_dir = dir.path().to_path_buf();
    teamtype_dir.push(".teamtype");
    sandbox::create_dir(dir.path(), &teamtype_dir).expect("Failed to create .teamtype directory");

    let file = dir.child("file");
    sandbox::write_file(dir.path(), &file, b"hello")
        .expect("Failed to create file in temp directory");

    let mut socket_path = dir.child(".teamtype");
    socket_path.push("socket");
    let mut socket = MockSocket::new(&socket_path);

    let uri = format!("file://{}", &file.display());

    println!(
        "Open {:?} with your editor. I'm expecting an open message.",
        &file
    );

    expect_request(
        &mut socket,
        EditorProtocolMessageFromEditor::Open {
            uri: uri.clone(),
            content: "hello".to_string(),
        },
    )
    .await;

    let response = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "result": "success"
    });
    socket.send(&response.to_string()).await;
    socket.send("\n").await;

    println!("Insert an 'x' at the beginning of the document.");

    expect_request(
        &mut socket,
        EditorProtocolMessageFromEditor::Edit {
            uri: uri.clone(),
            revision: 0,
            delta: EditorTextDelta(vec![EditorTextOp {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
                replacement: "x".to_string(),
            }]),
        },
    )
    .await;

    println!("Close the file. I'm expecting a close message.");

    // TODO: Our spec says this "should" be a request, but Neovim sends a notification...
    expect_notification(
        &mut socket,
        EditorProtocolMessageFromEditor::Close { uri: uri.clone() },
    )
    .await;

    println!("All tests passed! :)");
    Ok(())
}
