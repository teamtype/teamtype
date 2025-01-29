use anyhow::{bail, Result};
use ethersync::{
    ot::OTServer,
    path::RelativePath,
    types::{
        ComponentMessage, ContentLengthCodec, EditorProtocolMessageError,
        EditorProtocolMessageFromEditor, EditorProtocolMessageToEditor, EditorProtocolObject,
        EditorTextDelta, JSONRPCFromEditor, JSONRPCResponse, RevisionedEditorTextDelta,
        RevisionedTextDelta, TextDelta, TextOp,
    },
};
use futures::{SinkExt, StreamExt};
use futures_util::FutureExt;
use reqwest::cookie::{CookieStore, Jar};
use rust_socketio::{
    asynchronous::{Client, ClientBuilder},
    Payload, TransportType,
};
use serde_json::json;
use std::collections::VecDeque;
use std::io::{Read, Write};
use std::marker::PhantomData;
use std::sync::Arc;
use tokio::io::{BufReader, BufWriter};
use tokio::sync::mpsc;
use tokio::sync::Mutex;
use tokio_util::codec::FramedRead;
use tokio_util::codec::FramedWrite;

trait Pipe<InputFromIO, InputToIO, OutputFromIO, OutputToIO> {
    fn handle_input_from_io(&mut self, message: InputFromIO);
    fn handle_input_to_io(&mut self, message: InputToIO);
    fn poll_transmit_from_io(&mut self) -> Option<OutputFromIO>;
    fn poll_transmit_to_io(&mut self) -> Option<OutputToIO>;
}

struct Glue<A, B, InputFromIO, InputToIO, InputToA, OutputFromIO, OutputToIO, OutputFromA>
where
    A: Pipe<InputFromIO, InputToA, OutputFromA, OutputToIO>,
    B: Pipe<OutputFromA, InputToIO, OutputFromIO, InputToA>,
{
    a: A,
    b: B,
    _marker: PhantomData<(
        InputFromIO,
        InputToIO,
        InputToA,
        OutputFromIO,
        OutputToIO,
        OutputFromA,
    )>,
}

// new
impl<A, B, InputFromIO, InputToIO, InputToA, OutputFromIO, OutputToIO, OutputFromA>
    Glue<A, B, InputFromIO, InputToIO, InputToA, OutputFromIO, OutputToIO, OutputFromA>
where
    A: Pipe<InputFromIO, InputToA, OutputFromA, OutputToIO>,
    B: Pipe<OutputFromA, InputToIO, OutputFromIO, InputToA>,
{
    fn new(a: A, b: B) -> Self {
        Self {
            a,
            b,
            _marker: PhantomData,
        }
    }
}

// impl Pipe for Glue
impl<A, B, InputFromIO, InputToIO, InputToA, OutputFromIO, OutputToIO, OutputFromA>
    Pipe<InputFromIO, InputToIO, OutputFromIO, OutputToIO>
    for Glue<A, B, InputFromIO, InputToIO, InputToA, OutputFromIO, OutputToIO, OutputFromA>
where
    A: Pipe<InputFromIO, InputToA, OutputFromA, OutputToIO>,
    B: Pipe<OutputFromA, InputToIO, OutputFromIO, InputToA>,
{
    fn handle_input_from_io(&mut self, message: InputFromIO) {
        self.a.handle_input_from_io(message);
    }
    fn handle_input_to_io(&mut self, message: InputToIO) {
        self.b.handle_input_to_io(message);
    }
    fn poll_transmit_from_io(&mut self) -> Option<OutputFromIO> {
        while let Some(message) = self.a.poll_transmit_from_io() {
            self.b.handle_input_from_io(message);
        }
        self.b.poll_transmit_from_io()
    }

    fn poll_transmit_to_io(&mut self) -> Option<OutputToIO> {
        while let Some(message) = self.b.poll_transmit_to_io() {
            self.a.handle_input_to_io(message);
        }
        self.a.poll_transmit_to_io()
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

// Pipe that reads bytes, and outputs strings, split by newline
// The other direction is in reverse.
#[derive(Default)]
struct BytesToLinesPipe {
    input_from_io: Vec<u8>,
    input_to_io: Vec<String>,
}

impl Pipe<Vec<u8>, String, String, Vec<u8>> for BytesToLinesPipe {
    fn handle_input_from_io(&mut self, message: Vec<u8>) {
        self.input_from_io.extend(message);
    }
    fn handle_input_to_io(&mut self, message: String) {
        self.input_to_io.push(message);
    }
    fn poll_transmit_from_io(&mut self) -> Option<String> {
        if let Some(pos) = self.input_from_io.iter().position(|&x| x == b'\n') {
            let message = self.input_from_io.drain(..pos).collect();
            self.input_from_io.drain(..1);
            Some(String::from_utf8(message).unwrap())
        } else {
            None
        }
    }
    fn poll_transmit_to_io(&mut self) -> Option<Vec<u8>> {
        self.input_to_io.pop().map(|message| {
            let mut bytes = message.into_bytes();
            // append new line
            bytes.push(b'\n');
            bytes
        })
    }
}

// Pipe that reads strings, and parses them as numbers
// The other direction is in reverse.
#[derive(Default)]
struct StringToNumberPipe {
    input_from_io: Vec<String>,
    output_to_io: Vec<String>,
}

impl Pipe<String, i32, i32, String> for StringToNumberPipe {
    fn handle_input_from_io(&mut self, message: String) {
        self.input_from_io.push(message);
    }
    fn handle_input_to_io(&mut self, message: i32) {
        self.output_to_io.push(message.to_string());
    }
    fn poll_transmit_from_io(&mut self) -> Option<i32> {
        self.input_from_io.pop().and_then(|message| {
            if let Ok(n) = message.parse() {
                Some(n)
            } else {
                self.output_to_io.push("Invalid number".to_string());
                None
            }
        })
    }
    fn poll_transmit_to_io(&mut self) -> Option<String> {
        self.output_to_io.pop()
    }
}

fn main() {
    let mut bytes_to_numbers_pipe =
        Glue::new(BytesToLinesPipe::default(), StringToNumberPipe::default());
    let mut stdin = std::io::stdin().lock();

    loop {
        // poll from pipe
        while let Some(n) = bytes_to_numbers_pipe.poll_transmit_from_io() {
            // double
            let n = 2 * n;
            // write back to pipe
            bytes_to_numbers_pipe.handle_input_to_io(n);
        }

        while let Some(bytes) = bytes_to_numbers_pipe.poll_transmit_to_io() {
            // write to stdout
            std::io::stdout().write_all(&bytes).unwrap();
        }

        let buf = &mut [0; 100];
        let n = stdin.read(buf).unwrap();
        // feed to pipe
        bytes_to_numbers_pipe.handle_input_from_io(buf[..n].to_vec());
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

async fn get_cookie(url: &str) -> Result<String> {
    let cookie_jar = Arc::new(Jar::default());

    let client = reqwest::Client::builder()
        .cookie_store(true)
        .cookie_provider(cookie_jar.clone())
        .build()?;

    let response = client.get(url).send().await?;

    if !response.status().is_success() {
        bail!(format!("Failed to fetch URL: {}", response.status()));
    }

    if let Some(cookies) = cookie_jar.cookies(&url.parse()?) {
        // Return the cookies as a string
        Ok(cookies.to_str()?.to_string())
    } else {
        bail!("No cookies found.")
    }
}

struct HedgedocBinding {
    ot: OTServer,
    latest_revision: u64,
    buffered_transmits_to_hedgedoc: VecDeque<(String, Payload)>,
    buffered_transmits_to_editor: VecDeque<ComponentMessage>,
}

impl HedgedocBinding {
    fn new() -> Self {
        Self {
            ot: OTServer::new("".to_string()),
            latest_revision: 0,
            buffered_transmits_to_hedgedoc: VecDeque::new(),
            buffered_transmits_to_editor: VecDeque::new(),
        }
    }
    fn poll_transmit_to_hedgedoc(&mut self) -> Option<(String, Payload)> {
        self.buffered_transmits_to_hedgedoc.pop_front()
    }
    fn poll_transmit_to_editor(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_to_editor.pop_front()
    }
    fn handle_input(&mut self, (event, data): (String, Payload)) {
        match event.as_str() {
            "operation" => {
                if let Payload::Text(data) = data {
                    // Assume the edit is for the latest revision...
                    //let revision = data[1].as_u64().unwrap();
                    //if revision > self.latest_revision {
                    //    self.latest_revision = revision;
                    //}

                    // TODO: Move this conversion to types.rs.
                    let mut delta = TextDelta::default();
                    let mut l = 0;
                    for component in data[2].as_array().unwrap() {
                        match component {
                            serde_json::Value::Number(n) => {
                                let n = n.as_i64().unwrap();
                                if n > 0 {
                                    delta.retain(n as usize);
                                    l += n as usize;
                                } else {
                                    delta.delete((-n) as usize);
                                    l += (-n) as usize;
                                }
                            }
                            serde_json::Value::String(s) => {
                                delta.insert(s);
                            }
                            _ => {
                                panic!("unexpected component");
                            }
                        }
                    }
                    // Fill up until l == length of content
                    let content = self.ot.current_content();
                    let content_len = content.chars().count();
                    if l < content_len {
                        delta.retain(content_len - l);
                    }

                    let rev_delta = self.ot.apply_crdt_change(&delta);
                    let delta = RevisionedTextDelta::from_rev_ed_delta(
                        rev_delta,
                        &self.ot.current_content(),
                    )
                    .delta;

                    let message = ComponentMessage::Edit {
                        file_path: RelativePath::new("todo"),
                        delta,
                    };
                    self.buffered_transmits_to_editor.push_back(message);
                }
            }
            _ => {
                todo!();
            }
        }
    }
    fn handle_inner(&mut self, message: ComponentMessage) {
        match message {
            ComponentMessage::Edit { delta, .. } => {
                let revision = self.ot.daemon_revision;
                let rev_delta = RevisionedEditorTextDelta {
                    revision,
                    delta: EditorTextDelta::from_delta(delta.clone(), &self.ot.current_content()),
                };
                let (delta_for_crdt, rev_deltas_for_editor) =
                    self.ot.apply_editor_operation(rev_delta);

                let mut text = Vec::new();
                for op in delta_for_crdt.0 {
                    match op {
                        TextOp::Retain(n) => {
                            text.push(json!(n));
                        }
                        TextOp::Insert(s) => {
                            text.push(json!(s));
                        }
                        TextOp::Delete(n) => {
                            text.push(json!(-(n as i64)));
                        }
                    }
                }
                self.buffered_transmits_to_hedgedoc.push_back((
                    "operation".to_string(),
                    vec![
                        json!(self.ot.daemon_revision),
                        serde_json::Value::Array(text),
                        json!({"ranges": [{"anchor": 0, "head": 0}]}),
                    ]
                    .into(),
                ));

                // todo: process rev_deltas_for_editor?
            }
            _ => {
                todo!();
            }
        }
    }
}

struct InnerEditorBinding {
    ot: OTServer,
    buffered_transmits_to_hedgedoc: VecDeque<ComponentMessage>,
    buffered_transmits_to_editor: VecDeque<EditorProtocolMessageToEditor>,
}

impl InnerEditorBinding {
    fn new() -> Self {
        Self {
            ot: OTServer::new("".to_string()),
            buffered_transmits_to_hedgedoc: VecDeque::new(),
            buffered_transmits_to_editor: VecDeque::new(),
        }
    }
    fn poll_transmit_to_hedgedoc(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_to_hedgedoc.pop_front()
    }
    fn poll_transmit_to_editor(&mut self) -> Option<EditorProtocolMessageToEditor> {
        self.buffered_transmits_to_editor.pop_front()
    }
    fn handle_input(&mut self, message: EditorProtocolMessageFromEditor) -> Result<(), String> {
        match message {
            EditorProtocolMessageFromEditor::Open { .. } => {}
            EditorProtocolMessageFromEditor::Edit {
                delta, revision, ..
            } => {
                let rev_delta = RevisionedEditorTextDelta {
                    revision,
                    delta: delta.clone(),
                };
                let (delta_for_crdt, rev_deltas_for_editor) =
                    self.ot.apply_editor_operation(rev_delta);
                self.buffered_transmits_to_hedgedoc
                    .push_back(ComponentMessage::Edit {
                        file_path: RelativePath::new("todo"),
                        delta: delta_for_crdt,
                    });
                self.buffered_transmits_to_editor
                    .extend(rev_deltas_for_editor.into_iter().map(|rev_delta| {
                        EditorProtocolMessageToEditor::Edit {
                            uri: "file:///home/blinry/tmp/playground/file".to_string(),
                            revision: rev_delta.revision,
                            delta: rev_delta.delta,
                        }
                    }));
            }
            _ => {
                // todo
            }
        }
        Ok(())
    }
    fn handle_inner(&mut self, message: ComponentMessage) {
        match message {
            ComponentMessage::Edit { delta, .. } => {
                let rev_delta = self.ot.apply_crdt_change(&delta);
                self.buffered_transmits_to_editor
                    .push_back(EditorProtocolMessageToEditor::Edit {
                        uri: "file:///home/blinry/tmp/playground/file".to_string(),
                        revision: rev_delta.revision,
                        delta: rev_delta.delta,
                    });
            }
            _ => {
                todo!();
            }
        }
    }
}

struct EditorBinding {
    inner: InnerEditorBinding,
    buffered_transmits_to_hedgedoc: VecDeque<ComponentMessage>,
    buffered_transmits_to_editor: VecDeque<String>,
}

impl EditorBinding {
    fn new() -> Self {
        Self {
            inner: InnerEditorBinding::new(),
            buffered_transmits_to_hedgedoc: VecDeque::new(),
            buffered_transmits_to_editor: VecDeque::new(),
        }
    }
    fn poll_transmit_to_hedgedoc(&mut self) -> Option<ComponentMessage> {
        while let Some(message) = self.inner.poll_transmit_to_hedgedoc() {
            self.buffered_transmits_to_hedgedoc.push_back(message);
        }
        self.buffered_transmits_to_hedgedoc.pop_front()
    }
    fn poll_transmit_to_editor(&mut self) -> Option<String> {
        while let Some(message) = self.inner.poll_transmit_to_editor() {
            self.buffered_transmits_to_editor.push_back(
                EditorProtocolObject::Request(message)
                    .to_jsonrpc()
                    .expect("should work"),
            );
        }
        self.buffered_transmits_to_editor.pop_front()
    }
    fn handle_input(&mut self, message: String) {
        let parsed = JSONRPCFromEditor::from_jsonrpc(&message);
        if let Ok(parsed) = parsed {
            match parsed {
                JSONRPCFromEditor::Request { id, payload } => {
                    let result = self.inner.handle_input(payload);
                    let response = match result {
                        Err(error) => JSONRPCResponse::RequestError {
                            id: Some(id),
                            error: EditorProtocolMessageError {
                                code: -1,
                                message: error,
                                data: None,
                            },
                        },
                        Ok(_) => JSONRPCResponse::RequestSuccess {
                            id,
                            result: "success".into(),
                        },
                    };
                    let object = EditorProtocolObject::Response(response);
                    self.buffered_transmits_to_editor
                        .push_back(object.to_jsonrpc().expect("should work"));
                }
                JSONRPCFromEditor::Notification { payload } => {
                    let _ = self.inner.handle_input(payload);
                }
            }
        }
    }
    fn handle_inner(&mut self, message: ComponentMessage) {
        self.inner.handle_inner(message);
    }
}

async fn create_socket() -> (Client, tokio::sync::mpsc::Receiver<(String, Payload)>) {
    let server = "https://md.ha.si";
    let cookie = get_cookie(server).await.expect("Failed to get cookie");

    let (tx, rx) = mpsc::channel(32);

    let tx = Arc::new(Mutex::new(tx));

    let callback_tx = Arc::clone(&tx);
    let callback = move |payload: Payload, _socket: Client| {
        let callback_tx = Arc::clone(&callback_tx);
        async move {
            // Lock and send the message
            let tx = callback_tx.lock().await;
            if let Err(e) = tx.send(("operation".to_string(), payload)).await {
                eprintln!("Failed to send message to channel: {}", e);
            }
        }
        .boxed()
    };

    let socket = ClientBuilder::new(format!("{server}/socket.io/?noteId=test"))
        .transport_type(TransportType::Polling)
        .opening_header("Cookie", cookie)
        .on("operation", callback)
        .connect()
        .await
        .expect("Connection failed");

    (socket, rx)
}

/*#[tokio::main]
async fn main() {
    let (socket, mut rx) = create_socket().await;

    let mut stdin = FramedRead::new(BufReader::new(tokio::io::stdin()), ContentLengthCodec);
    let mut stdout = FramedWrite::new(BufWriter::new(tokio::io::stdout()), ContentLengthCodec);

    let mut hedgedoc = HedgedocBinding::new();
    let mut editor = EditorBinding::new();

    let mut running = true;
    while running {
        if let Some((event, data)) = hedgedoc.poll_transmit_to_hedgedoc() {
            socket.emit(event, data).await.expect("Failed to emit");
            continue;
        }
        if let Some(message) = hedgedoc.poll_transmit_to_editor() {
            editor.handle_inner(message);
        }
        if let Some(message) = editor.poll_transmit_to_hedgedoc() {
            hedgedoc.handle_inner(message);
            continue;
        }
        if let Some(message) = editor.poll_transmit_to_editor() {
            stdout
                .send(message)
                .await
                .expect("Failed to write to stdout");
            continue;
        }
        tokio::select! {
            socket_message_maybe = rx.recv() => {
                if let Some(socket_message) = socket_message_maybe {
                    hedgedoc.handle_input(socket_message);
                } else {
                    running = false;
                }
            }
            editor_message_maybe = stdin.next() => {
                if let Some(Ok(editor_message)) = editor_message_maybe {
                    editor.handle_input(editor_message.clone());
                } else {
                    running = false;
                }
            }
        }
    }
}
*/
