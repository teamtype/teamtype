use anyhow::{bail, Result};
use ethersync::{
    ot::OTServer,
    path::RelativePath,
    types::{
        ComponentMessage, EditorProtocolMessageError, EditorProtocolMessageFromEditor,
        EditorProtocolMessageToEditor, EditorProtocolObject, EditorTextDelta, EditorTextOp,
        JSONRPCFromEditor, JSONRPCResponse, Position, Range, RevisionedEditorTextDelta,
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
use tokio::io::{AsyncReadExt, AsyncWriteExt, BufReader, BufWriter};
use tokio::sync::mpsc;
use tokio::sync::Mutex;

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

macro_rules! glue {
     ( $a:expr, $b:expr $(, $more:expr )* $(,)? ) => {
        {
            let pipe = Glue::new($a, $b);
            $(
                let pipe = Glue::new(pipe, $more);
            )*
            pipe
        }
    };
}

struct Flip<A, InputFromIO, InputToIO, OutputFromIO, OutputToIO>
where
    A: Pipe<InputToIO, InputFromIO, OutputToIO, OutputFromIO>,
{
    pipe: A,
    _marker: PhantomData<(InputFromIO, InputToIO, OutputFromIO, OutputToIO)>,
}

impl<A, InputFromIO, InputToIO, OutputFromIO, OutputToIO>
    Flip<A, InputFromIO, InputToIO, OutputFromIO, OutputToIO>
where
    A: Pipe<InputToIO, InputFromIO, OutputToIO, OutputFromIO>,
{
    fn new(pipe: A) -> Self {
        Self {
            pipe,
            _marker: PhantomData,
        }
    }
}

impl<A, InputFromIO, InputToIO, OutputFromIO, OutputToIO>
    Pipe<InputFromIO, InputToIO, OutputFromIO, OutputToIO>
    for Flip<A, InputFromIO, InputToIO, OutputFromIO, OutputToIO>
where
    A: Pipe<InputToIO, InputFromIO, OutputToIO, OutputFromIO>,
{
    fn handle_input_from_io(&mut self, message: InputFromIO) {
        self.pipe.handle_input_to_io(message);
    }
    fn handle_input_to_io(&mut self, message: InputToIO) {
        self.pipe.handle_input_from_io(message);
    }
    fn poll_transmit_from_io(&mut self) -> Option<OutputFromIO> {
        self.pipe.poll_transmit_to_io()
    }
    fn poll_transmit_to_io(&mut self) -> Option<OutputToIO> {
        self.pipe.poll_transmit_from_io()
    }
}

struct Log<Forward, Backward> {
    buffered_transmits_from_io: VecDeque<Forward>,
    buffered_transmits_to_io: VecDeque<Backward>,
    log_file: std::fs::File,
}

impl<Forward, Backward> Log<Forward, Backward> {
    fn new(log_file_path: &str) -> Self {
        Self {
            buffered_transmits_from_io: VecDeque::new(),
            buffered_transmits_to_io: VecDeque::new(),
            log_file: std::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(log_file_path)
                .expect("Unable to open log file"),
        }
    }
}

impl<Forward, Backward> Pipe<Forward, Backward, Forward, Backward> for Log<Forward, Backward>
where
    Forward: std::fmt::Debug,
    Backward: std::fmt::Debug,
{
    fn handle_input_from_io(&mut self, message: Forward) {
        self.log_file
            .write_all(format!(">> {:?}\n", message).as_bytes())
            .expect("Should be able to write to log file");
        self.log_file
            .flush()
            .expect("Should be able to flush the log file");
        self.buffered_transmits_from_io.push_back(message);
    }
    fn handle_input_to_io(&mut self, message: Backward) {
        self.log_file
            .write_all(format!("<< {:?}\n", message).as_bytes())
            .expect("Should be able to write to log file");
        self.log_file
            .flush()
            .expect("Should be able to flush the log file");
        self.buffered_transmits_to_io.push_back(message);
    }
    fn poll_transmit_from_io(&mut self) -> Option<Forward> {
        self.buffered_transmits_from_io.pop_front()
    }
    fn poll_transmit_to_io(&mut self) -> Option<Backward> {
        self.buffered_transmits_to_io.pop_front()
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
struct ContentLengthCodec {
    input_from_io: String,
    input_to_io: String,
}

impl Pipe<String, String, String, String> for ContentLengthCodec {
    fn handle_input_from_io(&mut self, message: String) {
        self.input_from_io.push_str(&message);
    }
    fn handle_input_to_io(&mut self, message: String) {
        let content_length = message.len();
        let string = format!("Content-Length: {}\r\n\r\n", content_length);
        self.input_to_io.push_str(&string);
        self.input_to_io.push_str(&message);
    }
    fn poll_transmit_to_io(&mut self) -> Option<String> {
        if self.input_to_io.is_empty() {
            None
        } else {
            Some(self.input_to_io.drain(..).collect())
        }
    }
    fn poll_transmit_from_io(&mut self) -> Option<String> {
        // Find the position of the Content-Length header.
        let content_length_header = b"Content-Length: ";
        let content_length_header_len = content_length_header.len();
        let start_of_header = match self
            .input_from_io
            .as_bytes()
            .windows(content_length_header_len)
            .position(|window| window == content_length_header)
        {
            Some(pos) => pos,
            None => return None,
        };

        // Find the end of the line after that.
        let (end_of_line, end_of_line_bytes) = match self.input_from_io
            [start_of_header + content_length_header.len()..]
            .as_bytes()
            .windows(4)
            .position(|window| window == b"\r\n\r\n")
        {
            Some(pos) => (pos, 4),
            // Even though this is not valid in terms of the spec, also
            // accept plain newline separators in order to simplify manual testing.
            None => match self.input_from_io[start_of_header + content_length_header.len()..]
                .as_bytes()
                .windows(2)
                .position(|window| window == b"\n\n")
            {
                Some(pos) => (pos, 2),
                None => return None,
            },
        };

        // Parse the content length.
        let content_length = &self.input_from_io[start_of_header + content_length_header.len()
            ..start_of_header + content_length_header.len() + end_of_line]
            .parse::<usize>()
            .unwrap();
        let content_start =
            start_of_header + content_length_header.len() + end_of_line + end_of_line_bytes;

        // Check if we have enough content.
        if self.input_from_io.len() < content_start + content_length {
            return None;
        }

        // Discard the header.
        self.input_from_io.drain(..content_start);
        // Return the content.
        Some(self.input_from_io.drain(..content_length).collect())
    }
}

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

#[derive(Default)]
struct LinesCodec {
    input_from_io: String,
    output_to_io: String,
}

impl Pipe<String, String, String, String> for LinesCodec {
    fn handle_input_from_io(&mut self, message: String) {
        self.input_from_io.push_str(&message);
    }
    fn handle_input_to_io(&mut self, message: String) {
        self.output_to_io.push_str(&message);
        self.output_to_io.push('\n');
    }
    fn poll_transmit_from_io(&mut self) -> Option<String> {
        if let Some(pos) = self.input_from_io.find('\n') {
            let message = self.input_from_io.drain(..pos).collect();
            self.input_from_io.drain(..1);
            Some(message)
        } else {
            None
        }
    }
    fn poll_transmit_to_io(&mut self) -> Option<String> {
        if self.output_to_io.is_empty() {
            None
        } else {
            Some(self.output_to_io.drain(..).collect())
        }
    }
}

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

/*fn main() {
    let mut bytes_to_numbers_pipe =
        Glue::new(BytesToLinesPipe::default(), StringToNumberPipe::default());
    let mut stdin = std::io::stdin().lock();

    loop {
        // poll from pipe
        if let Some(n) = bytes_to_numbers_pipe.poll_transmit_from_io() {
            // double
            let n = 2 * n;
            // write back to pipe
            bytes_to_numbers_pipe.handle_input_to_io(n);
            continue;
        }

        if let Some(bytes) = bytes_to_numbers_pipe.poll_transmit_to_io() {
            // write to stdout
            std::io::stdout().write_all(&bytes).unwrap();
            continue;
        }

        let buf = &mut [0; 100];
        let n = stdin.read(buf).unwrap();
        // feed to pipe
        bytes_to_numbers_pipe.handle_input_from_io(buf[..n].to_vec());
    }
}*/

//////////////////////////////////////////////////////////////////////////////////////////////////

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

#[derive(Default)]
struct HedgedocBinding {
    latest_revision: u64,
    buffered_transmits_to_hedgedoc: VecDeque<(String, Payload)>,
    buffered_transmits_to_editor: VecDeque<ComponentMessage>,
}

impl Pipe<(String, Payload), ComponentMessage, ComponentMessage, (String, Payload)>
    for HedgedocBinding
{
    fn poll_transmit_to_io(&mut self) -> Option<(String, Payload)> {
        self.buffered_transmits_to_hedgedoc.pop_front()
    }
    fn poll_transmit_from_io(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_to_editor.pop_front()
    }
    fn handle_input_from_io(&mut self, (event, data): (String, Payload)) {
        match event.as_str() {
            "operation" => {
                if let Payload::Text(data) = data {
                    // Assume the edit is for the latest revision...
                    let revision = data[1].as_u64().unwrap();
                    if revision > self.latest_revision {
                        self.latest_revision = revision;
                    }

                    // TODO: Move this conversion to types.rs.
                    let mut delta = TextDelta::default();
                    for component in data[2].as_array().unwrap() {
                        match component {
                            serde_json::Value::Number(n) => {
                                let n = n.as_i64().unwrap();
                                if n > 0 {
                                    delta.retain(n as usize);
                                } else {
                                    delta.delete((-n) as usize);
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

                    let message = ComponentMessage::Edit {
                        file_path: RelativePath::new("file"),
                        delta,
                    };
                    self.buffered_transmits_to_editor.push_back(message);
                }
            }
            "doc" => {
                if let Payload::Text(data) = data {
                    let map = data[0].as_object().unwrap();
                    if let Some(str) = map.get("str") {
                        let content = str.as_str().unwrap();
                        let open = ComponentMessage::Open {
                            file_path: RelativePath::new("file"),
                            content: content.to_string(),
                        };
                        self.buffered_transmits_to_editor.push_back(open);
                    }
                    if let Some(n) = map.get("revision") {
                        self.latest_revision = n.as_u64().unwrap();
                    }
                }
            }
            "cursor activity" => {
                if let Payload::Text(data) = data {
                    let map = data[0].as_object().unwrap();

                    let name = map.get("name").unwrap().as_str().unwrap();
                    let id = map.get("id").unwrap().as_str().unwrap();

                    // parse the format
                    let cursor = map.get("cursor").unwrap().as_object().unwrap();
                    let line = cursor.get("line").unwrap().as_u64().unwrap() as usize;
                    let ch = cursor.get("ch").unwrap().as_u64().unwrap() as usize;
                    let range = Range {
                        start: Position {
                            line,
                            character: ch,
                        },
                        end: Position {
                            line,
                            character: ch,
                        },
                    };

                    let message = ComponentMessage::Cursor {
                        cursor_id: id.to_string(),
                        name: Some(name.to_string()),
                        file_path: RelativePath::new("file"),
                        ranges: vec![range],
                    };
                    self.buffered_transmits_to_editor.push_back(message);
                }
            }
            _ => {
                todo!();
            }
        }
    }
    fn handle_input_to_io(&mut self, message: ComponentMessage) {
        match message {
            ComponentMessage::Edit { delta, .. } => {
                //let revision = self.ot.daemon_revision;
                //let rev_delta = RevisionedEditorTextDelta {
                //    revision,
                //    delta: EditorTextDelta::from_delta(delta.clone(), &self.ot.current_content()),
                //};
                //let (delta_for_crdt, rev_deltas_for_editor) =
                //    self.ot.apply_editor_operation(rev_delta);

                let mut text = Vec::new();
                for op in delta.0 {
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
                        json!(self.latest_revision),
                        serde_json::Value::Array(text),
                        json!({"ranges": [{"anchor": 0, "head": 0}]}),
                    ]
                    .into(),
                ));
                self.latest_revision += 1;

                // todo: process rev_deltas_for_editor?
            }
            ComponentMessage::Cursor { ranges, .. } => {
                let range = ranges[0].clone();
                let message = json!({
                    "line": range.start.line as u64,
                    "ch": range.start.character as u64,
                    "sticky": "after",
                    "xRel": 3.25, // ???
                });
                //https://md.ha.si/socket.io/?noteId=test&EIO=3&transport=polling&t=PJDp9Jn&sid=8LNDtCS1OEzstE25ADiw
                //65:42["cursor focus",{"line":3,"ch":7,"sticky":"after","xRel":3.25}]68:42["cursor activity",{"line":3,"ch":7,"sticky":"after","xRel":3.25}]
                self.buffered_transmits_to_hedgedoc
                    .push_back(("cursor activity".to_string(), vec![message].into()));
            }
            _ => {
                // pass
            }
        }
    }
}

struct OneSidedOT {
    ot: Option<OTServer>,
    buffered_transmits_to_hedgedoc: VecDeque<ComponentMessage>,
    buffered_transmits_to_editor: VecDeque<EditorProtocolMessageToEditor>,
}

impl OneSidedOT {
    fn new() -> Self {
        Self {
            ot: None,
            buffered_transmits_to_hedgedoc: VecDeque::new(),
            buffered_transmits_to_editor: VecDeque::new(),
        }
    }
}

impl
    Pipe<
        EditorProtocolMessageFromEditor,
        ComponentMessage,
        ComponentMessage,
        EditorProtocolMessageToEditor,
    > for OneSidedOT
{
    fn poll_transmit_from_io(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_to_hedgedoc.pop_front()
    }
    fn poll_transmit_to_io(&mut self) -> Option<EditorProtocolMessageToEditor> {
        self.buffered_transmits_to_editor.pop_front()
    }
    fn handle_input_from_io(&mut self, message: EditorProtocolMessageFromEditor) {
        match message {
            EditorProtocolMessageFromEditor::Open { content, .. } => {
                self.ot = Some(OTServer::new(content.clone()));
                self.buffered_transmits_to_hedgedoc
                    .push_back(ComponentMessage::Open {
                        file_path: RelativePath::new("file"),
                        content,
                    });
            }
            EditorProtocolMessageFromEditor::Edit {
                delta, revision, ..
            } => {
                if let Some(ot) = &mut self.ot {
                    let rev_delta = RevisionedEditorTextDelta {
                        revision,
                        delta: delta.clone(),
                    };
                    let (delta_for_crdt, rev_deltas_for_editor) =
                        ot.apply_editor_operation(rev_delta);
                    self.buffered_transmits_to_hedgedoc
                        .push_back(ComponentMessage::Edit {
                            file_path: RelativePath::new("file"),
                            delta: delta_for_crdt,
                        });
                    self.buffered_transmits_to_editor.extend(
                        rev_deltas_for_editor.into_iter().map(|rev_delta| {
                            EditorProtocolMessageToEditor::Edit {
                                uri: "file:///home/blinry/tmp/playground/file".to_string(),
                                revision: rev_delta.revision,
                                delta: rev_delta.delta,
                            }
                        }),
                    );
                }
            }
            EditorProtocolMessageFromEditor::Cursor { ranges, .. } => {
                self.buffered_transmits_to_hedgedoc
                    .push_back(ComponentMessage::Cursor {
                        cursor_id: "todo".to_string(),
                        name: Some("todo".to_string()),
                        file_path: RelativePath::new("file"),
                        ranges,
                    });
            }
            _ => {
                // todo
            }
        }
    }
    fn handle_input_to_io(&mut self, message: ComponentMessage) {
        match message {
            ComponentMessage::Edit { delta, .. } => {
                if let Some(ot) = &mut self.ot {
                    let rev_delta = ot.apply_crdt_change(&delta);
                    self.buffered_transmits_to_editor.push_back(
                        EditorProtocolMessageToEditor::Edit {
                            uri: "file:///home/blinry/tmp/playground/file".to_string(),
                            revision: rev_delta.revision,
                            delta: rev_delta.delta,
                        },
                    );
                }
            }
            ComponentMessage::Cursor {
                ranges,
                name,
                cursor_id,
                ..
            } => {
                self.buffered_transmits_to_editor
                    .push_back(EditorProtocolMessageToEditor::Cursor {
                        ranges,
                        name,
                        userid: cursor_id,
                        uri: "file:///home/blinry/tmp/playground/file".to_string(),
                    })
            }
            _ => {
                todo!();
            }
        }
    }
}

#[derive(Default)]
struct AutoAcceptingJsonRpc {
    buffered_transmits_to_hedgedoc: VecDeque<EditorProtocolMessageFromEditor>,
    buffered_transmits_to_editor: VecDeque<String>,
}

impl Pipe<String, EditorProtocolMessageToEditor, EditorProtocolMessageFromEditor, String>
    for AutoAcceptingJsonRpc
{
    fn poll_transmit_from_io(&mut self) -> Option<EditorProtocolMessageFromEditor> {
        self.buffered_transmits_to_hedgedoc.pop_front()
    }
    fn poll_transmit_to_io(&mut self) -> Option<String> {
        self.buffered_transmits_to_editor.pop_front()
    }
    fn handle_input_from_io(&mut self, message: String) {
        let parsed = JSONRPCFromEditor::from_jsonrpc(&message).expect("should work");
        let payload = match parsed {
            JSONRPCFromEditor::Request { id, payload } => {
                // Auto-acknowledge requests (TODO, might not make sense in other settings)
                let response = JSONRPCResponse::RequestSuccess {
                    id,
                    result: "success".into(),
                };
                self.buffered_transmits_to_editor.push_back(
                    EditorProtocolObject::Response(response)
                        .to_jsonrpc()
                        .expect("should work"),
                );
                payload
            }
            JSONRPCFromEditor::Notification { payload } => payload,
        };

        self.buffered_transmits_to_hedgedoc.push_back(payload);
    }
    fn handle_input_to_io(&mut self, message: EditorProtocolMessageToEditor) {
        self.buffered_transmits_to_editor.push_back(
            EditorProtocolObject::Request(message)
                .to_jsonrpc()
                .expect("should work"),
        );
    }
}

#[derive(Default)]
struct Truth {
    content: Option<String>,
    buffered_transmits_to_front: VecDeque<ComponentMessage>,
    buffered_transmits_to_back: VecDeque<ComponentMessage>,
}

impl Truth {
    fn handle(
        source: &mut VecDeque<ComponentMessage>,
        other: &mut VecDeque<ComponentMessage>,
        true_content: &mut Option<String>,
        message: ComponentMessage,
    ) {
        match &message {
            ComponentMessage::Cursor { .. } => {
                // forward
                other.push_back(message.clone());
            }
            ComponentMessage::Edit { delta, .. } => {
                // forward
                other.push_back(message.clone());
                if let Some(true_content_inner) = true_content {
                    *true_content = Some(delta.apply(&true_content_inner));
                }
            }
            ComponentMessage::Open { content, .. } => {
                if let Some(true_content) = true_content {
                    if *true_content == *content {
                        // All good.
                    } else {
                        let chunks = dissimilar::diff(content, true_content);
                        if let [] | [dissimilar::Chunk::Equal(_)] = chunks.as_slice() {
                            // nothing to do
                        }

                        let text_delta: TextDelta = chunks.into();
                        source.push_back(ComponentMessage::Edit {
                            file_path: RelativePath::new("file"),
                            delta: text_delta,
                        });
                    }
                } else {
                    *true_content = Some(content.to_string());
                }
            }
            _ => {}
        }
    }
}

impl Pipe<ComponentMessage, ComponentMessage, ComponentMessage, ComponentMessage> for Truth {
    fn poll_transmit_from_io(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_to_back.pop_front()
    }
    fn poll_transmit_to_io(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_to_front.pop_front()
    }
    fn handle_input_from_io(&mut self, message: ComponentMessage) {
        Truth::handle(
            &mut self.buffered_transmits_to_front,
            &mut self.buffered_transmits_to_back,
            &mut self.content,
            message,
        );
    }
    fn handle_input_to_io(&mut self, message: ComponentMessage) {
        Truth::handle(
            &mut self.buffered_transmits_to_back,
            &mut self.buffered_transmits_to_front,
            &mut self.content,
            message,
        );
    }
}

#[derive(Default)]
struct Debugger {
    buffered_transmits_to_io: VecDeque<String>,
    buffered_transmits_from_io: VecDeque<ComponentMessage>,
}

impl Pipe<String, ComponentMessage, ComponentMessage, String> for Debugger {
    fn poll_transmit_from_io(&mut self) -> Option<ComponentMessage> {
        self.buffered_transmits_from_io.pop_front()
    }
    fn poll_transmit_to_io(&mut self) -> Option<String> {
        self.buffered_transmits_to_io.pop_front()
    }
    fn handle_input_from_io(&mut self, message: String) {
        let components = message.split(" ").collect::<Vec<_>>();
        let message = match components[0] {
            "open" => {
                let file_path = RelativePath::new(components[1]);
                let content = components[2..].join(" ");
                ComponentMessage::Open { file_path, content }
            }
            "edit" => {
                let file_path = RelativePath::new("file");
                let mut delta = TextDelta::default();
                // loop through components[1..] and parse them into TextDelta
                // "..." are insertions, negative numbers are deletions, positive numbers are retain
                for component in components[1..].iter() {
                    if let Ok(n) = component.parse::<i32>() {
                        if n > 0 {
                            delta.retain(n as usize);
                        } else {
                            delta.delete((-n) as usize);
                        }
                    } else {
                        delta.insert(component);
                    }
                }
                ComponentMessage::Edit { file_path, delta }
            }
            _ => {
                todo!();
            }
        };
        self.buffered_transmits_from_io.push_back(message);
    }
    fn handle_input_to_io(&mut self, message: ComponentMessage) {
        self.buffered_transmits_to_io
            .push_back(format!("{:?}\n", message));
    }
}

#[derive(Default)]
struct EditorDebugger {
    buffered_transmits_to_io: VecDeque<String>,
    buffered_transmits_from_io: VecDeque<EditorProtocolMessageFromEditor>,
}

impl Pipe<String, EditorProtocolMessageToEditor, EditorProtocolMessageFromEditor, String>
    for EditorDebugger
{
    fn poll_transmit_from_io(&mut self) -> Option<EditorProtocolMessageFromEditor> {
        self.buffered_transmits_from_io.pop_front()
    }
    fn poll_transmit_to_io(&mut self) -> Option<String> {
        self.buffered_transmits_to_io.pop_front()
    }
    fn handle_input_from_io(&mut self, message: String) {
        let components = message.split(" ").collect::<Vec<_>>();
        let uri = RelativePath::new("file:///todo").to_string();
        let message = match components[0] {
            "open" => {
                let content = components[1..].join(" ");
                EditorProtocolMessageFromEditor::Open { uri, content }
            }
            "edit" => {
                let revision = components[1].parse().unwrap();
                let range = Range {
                    start: Position {
                        line: components[2].parse().unwrap(),
                        character: components[3].parse().unwrap(),
                    },
                    end: Position {
                        line: components[4].parse().unwrap(),
                        character: components[5].parse().unwrap(),
                    },
                };
                let replacement = components[6..].join(" ");
                let editor_delta = EditorTextDelta(vec![EditorTextOp { range, replacement }]);
                EditorProtocolMessageFromEditor::Edit {
                    uri,
                    revision,
                    delta: editor_delta,
                }
            }
            _ => {
                todo!();
            }
        };
        self.buffered_transmits_from_io.push_back(message);
    }
    fn handle_input_to_io(&mut self, message: EditorProtocolMessageToEditor) {
        self.buffered_transmits_to_io
            .push_back(format!("{:?}\n", message));
    }
}

async fn create_socket(
    hedgedoc_url: &str,
) -> (Client, tokio::sync::mpsc::Receiver<(String, Payload)>) {
    let server = hedgedoc_url
        .split("/")
        .take(3)
        .collect::<Vec<_>>()
        .join("/");
    let note = hedgedoc_url.split("/").nth(3).unwrap();

    let cookie = get_cookie(&server).await.expect("Failed to get cookie");

    let (tx, rx) = mpsc::channel(32);

    let tx = Arc::new(Mutex::new(tx));

    let callback_tx = Arc::clone(&tx);
    let operation_callback = move |payload: Payload, _socket: Client| {
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

    let callback_tx = Arc::clone(&tx);
    let doc_callback = move |payload: Payload, _socket: Client| {
        let callback_tx = Arc::clone(&callback_tx);
        async move {
            // Lock and send the message
            let tx = callback_tx.lock().await;
            if let Err(e) = tx.send(("doc".to_string(), payload)).await {
                eprintln!("Failed to send message to channel: {}", e);
            }
        }
        .boxed()
    };

    let callback_tx = Arc::clone(&tx);
    let cursor_callback = move |payload: Payload, _socket: Client| {
        let callback_tx = Arc::clone(&callback_tx);
        async move {
            // Lock and send the message
            let tx = callback_tx.lock().await;
            if let Err(e) = tx.send(("cursor activity".to_string(), payload)).await {
                eprintln!("Failed to send message to channel: {}", e);
            }
        }
        .boxed()
    };

    let socket = ClientBuilder::new(format!("{server}/socket.io/?noteId={note}"))
        .transport_type(TransportType::Polling)
        .opening_header("Cookie", cookie)
        .on("operation", operation_callback)
        .on("doc", doc_callback)
        .on("cursor activity", cursor_callback)
        .connect()
        .await
        .expect("Connection failed");

    (socket, rx)
}

#[tokio::main]
async fn main() {
    let hedgedoc_url = std::env::args()
        .nth(1)
        .expect("Please provide the Hedgedoc URL");

    let (socket, mut rx) = create_socket(&hedgedoc_url).await;

    let editor = glue![
        ContentLengthCodec::default(),
        AutoAcceptingJsonRpc::default(),
    ];

    //let editor = glue![LinesCodec::default(), EditorDebugger::default()];

    let editor_pipeline = glue![editor, OneSidedOT::new()];

    let mut buf = vec![0; 1024];
    let mut stdin = tokio::io::stdin();

    let hedgedoc_pipeline = glue![Log::new("/tmp/hedgedoclog"), HedgedocBinding::default()];

    let truth = Truth::default();

    let mut pipeline = glue![editor_pipeline, truth, Flip::new(hedgedoc_pipeline)];

    // Process first message from Hedgedoc direction, so that it determines the content.
    let socket_message = rx.recv().await.unwrap();
    pipeline.handle_input_to_io(socket_message);

    // Then, start the full pipeline.
    let mut running = true;
    while running {
        if let Some((event, data)) = pipeline.poll_transmit_from_io() {
            socket.emit(event, data).await.expect("Failed to emit");
            continue;
        }
        if let Some(message) = pipeline.poll_transmit_to_io() {
            print!("{message}");
            std::io::stdout().flush().unwrap();
            continue;
        }
        tokio::select! {
            socket_message_maybe = rx.recv() => {
                if let Some(socket_message) = socket_message_maybe {
                    pipeline.handle_input_to_io(socket_message);
                } else {
                    running = false;
                }
            }
            result = stdin.read(&mut buf) => {
                match result {
                    Ok(0) => {
                        // eof
                        running = false;
                    }
                    Ok(n) => {
                        pipeline.handle_input_from_io(String::from_utf8(buf[..n].to_vec()).unwrap());
                    }
                    Err(e) => {
                        eprintln!("Error reading from stdin: {}", e);
                        running = false;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truth() {
        let mut truth = Truth::default();
        truth.handle_input_from_io(ComponentMessage::Open {
            file_path: RelativePath::new("file"),
            content: "foo".to_string(),
        });
        assert_eq!(truth.content, Some("foo".to_string()));
        truth.handle_input_to_io(ComponentMessage::Open {
            file_path: RelativePath::new("file"),
            content: "oo".to_string(),
        });
        assert_eq!(truth.content, Some("foo".to_string()));
        let from_io = truth.poll_transmit_from_io().unwrap();
    }
}
