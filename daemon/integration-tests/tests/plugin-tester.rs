use anyhow::Result;
use pretty_assertions::assert_eq;
use teamtype::{
    cli_ask::ask,
    editor_protocol::{
        EditorProtocolMessageFromEditor, EditorProtocolMessageToEditor, IncomingMessage,
        OutgoingMessage,
    },
    sandbox,
    types::{EditorTextDelta, EditorTextOp, Position, Range},
};
use teamtype_integration_tests::socket::MockSocket;

fn instr(instruction: &str) {
    println!("[ ] {instruction}.");
}

fn check() {
    println!("\x1B[1A\x1B[2G\x1B[32m✓\x1B[0m");
}

async fn expect_request(
    instruction: &str,
    socket: &mut MockSocket,
    expected: EditorProtocolMessageFromEditor,
) {
    instr(instruction);
    loop {
        let msg = socket.recv().await;

        let message: IncomingMessage =
            serde_json::from_str(&msg.to_string()).expect("Could not parse EditorProtocolMessage");

        if let IncomingMessage::Request {
            payload: message,
            id,
        } = message
        {
            if let EditorProtocolMessageFromEditor::Cursor { .. } = message {
                // Ignore cursor messages.
                continue;
            }
            assert_eq!(expected, message);
            check();

            // Auto-confirm.
            let response = serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": "success"
            });
            socket.send(&response.to_string()).await;
            socket.send("\n").await;
        } else {
            panic!("Expected Request, got Notification");
        }
        return;
    }
}

// TODO: Avoid the duplication with expect_request.
async fn expect_notification(
    instruction: &str,
    socket: &mut MockSocket,
    expected: EditorProtocolMessageFromEditor,
) {
    instr(instruction);
    loop {
        let msg = socket.recv().await;

        let message: IncomingMessage =
            serde_json::from_str(&msg.to_string()).expect("Could not parse EditorProtocolMessage");

        if let IncomingMessage::Notification {
            payload: message, ..
        } = message
        {
            assert_eq!(expected, message);
            check();
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

async fn send_notification_and_ask(
    question: &str,
    socket: &mut MockSocket,
    message: EditorProtocolMessageToEditor,
) {
    let payload = OutgoingMessage::Notification(message).to_jsonrpc().unwrap();
    socket.send(&format!("{payload}\n")).await;

    if ask(&format!("[ ] {question}")).unwrap() {
        check();
    } else {
        panic!("This should have worked. :(");
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

    expect_request(
        &format!("Open {:?} with your editor", &file),
        &mut socket,
        EditorProtocolMessageFromEditor::Open {
            uri: uri.clone(),
            content: "hello".to_string(),
        },
    )
    .await;

    expect_request(
        "Insert an 'x' at the beginning of the document",
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

    // TODO: Our spec says this "should" be a request, but Neovim sends a notification...
    send_notification_and_ask(
        "Was a 'y' inserted after the 'hello'?",
        &mut socket,
        EditorProtocolMessageToEditor::Edit {
            uri: uri.clone(),
            revision: 1,
            delta: EditorTextDelta(vec![EditorTextOp {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 6,
                    },
                    end: Position {
                        line: 0,
                        character: 6,
                    },
                },
                replacement: "y".to_string(),
            }]),
        },
    )
    .await;

    // TODO: Our spec says this "should" be a request, but Neovim sends a notification...
    expect_notification(
        "Close the file. I'm expecting a close message",
        &mut socket,
        EditorProtocolMessageFromEditor::Close { uri: uri.clone() },
    )
    .await;

    println!("All tests passed! :)");
    Ok(())
}
