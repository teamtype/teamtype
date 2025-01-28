use anyhow::{bail, Result};
use futures_util::FutureExt;
use futures_util::StreamExt;
use reqwest::cookie::{CookieStore, Jar};
use rust_socketio::{
    asynchronous::{Client, ClientBuilder},
    Payload, TransportType,
};
use serde_json::json;
use std::collections::VecDeque;
use std::sync::Arc;
use tokio::io::BufReader;
use tokio::sync::mpsc;
use tokio::sync::Mutex;
use tokio_util::codec::FramedRead;
use tokio_util::codec::LinesCodec;

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
    latest_revision: u64,
    buffered_transmits: VecDeque<(String, Payload)>,
}

impl HedgedocBinding {
    fn new() -> Self {
        Self {
            latest_revision: 0,
            buffered_transmits: VecDeque::new(),
        }
    }
    fn poll_transmit(&mut self) -> Option<(String, Payload)> {
        self.buffered_transmits.pop_front()
    }
    fn handle_input(&mut self, (event, data): (String, Payload)) {
        dbg!(&event, &data);
        match event.as_str() {
            "operation" => {
                if let Payload::Text(data) = data {
                    let revision = data[1].as_u64().unwrap();
                    if revision > self.latest_revision {
                        self.latest_revision = revision;
                    }
                }
            }
            _ => {
                todo!();
            }
        }
    }
    fn insert(&mut self, text: String) {
        self.buffered_transmits.push_back((
            "operation".to_string(),
            vec![
                json!(self.latest_revision),
                json!([text, 1]), // needs to have proper "length"!
                json!({"ranges": [{"anchor": 0, "head": 0}]}),
            ]
            .into(),
        ));
    }
}

struct EditorBinding {}

impl EditorBinding {
    fn new() -> Self {
        Self {}
    }
    fn poll_transmit(&mut self) -> Option<String> {
        None
    }
    fn handle_input(&mut self, message: String) {
        dbg!(message);
    }
}

async fn create_socket() -> (Client, tokio::sync::mpsc::Receiver<(String, Payload)>) {
    let server = "https://md.ha.si";
    let cookie = get_cookie(server).await.expect("Failed to get cookie");
    dbg!(&cookie);

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

#[tokio::main]
async fn main() {
    let (socket, mut rx) = create_socket().await;
    let mut editor_stream = FramedRead::new(BufReader::new(tokio::io::stdin()), LinesCodec::new());

    let mut hedgedoc = HedgedocBinding::new();
    let mut editor = EditorBinding::new();

    let mut running = true;
    while running {
        if let Some((event, data)) = hedgedoc.poll_transmit() {
            dbg!(&event, &data);
            socket.emit(event, data).await.expect("Failed to emit");
            continue;
        }
        if let Some(message) = editor.poll_transmit() {
            println!("{}", message);
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
            editor_message_maybe = editor_stream.next() => {
                if let Some(Ok(editor_message)) = editor_message_maybe {
                    editor.handle_input(editor_message.clone());
                    hedgedoc.insert(editor_message);
                } else {
                    running = false;
                }
            }
        }
    }
}
