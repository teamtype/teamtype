// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::Path;

use serde_json::Value as JSONValue;
use teamtype::sandbox;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader, BufWriter, split};
#[cfg(unix)]
use tokio::net::UnixListener;
use tokio::sync::mpsc;
use tokio::time::Duration;

pub struct MockListener {
    writer_tx: mpsc::Sender<String>,
    reader_rx: mpsc::Receiver<String>,
}

#[cfg(unix)]
impl MockListener {
    pub fn new(listener_path: &Path) -> Self {
        let listener_dir = listener_path
            .parent()
            .expect("The constructed socket paths should be in a directory");
        if sandbox::exists(listener_dir, listener_path)
            .expect("Could not check for socket existence")
        {
            sandbox::remove_file(listener_dir, listener_path).expect("Could not remove socket");
        }

        let listener = UnixListener::bind(listener_path).expect("Could not bind to socket");
        let (writer_tx, mut writer_rx) = mpsc::channel::<String>(1);
        let (reader_tx, reader_rx) = mpsc::channel::<String>(1);

        tokio::spawn(async move {
            let (socket, _) = listener
                .accept()
                .await
                .expect("Could not accept connection");

            let (reader, writer) = split(socket);
            let mut writer = BufWriter::new(writer);
            let mut reader = BufReader::new(reader);

            tokio::spawn(async move {
                while let Some(message) = writer_rx.recv().await {
                    writer
                        .write_all(message.as_bytes())
                        .await
                        .expect("Could not write to socket");
                    writer.flush().await.expect("Could not flush socket");
                }
            });

            tokio::spawn(async move {
                let mut buffer = String::new();
                while reader.read_line(&mut buffer).await.is_ok() {
                    reader_tx
                        .send(buffer.clone())
                        .await
                        .expect("Could not send message to reader channel");
                    buffer.clear();
                }
            });
        });

        Self {
            writer_tx,
            reader_rx,
        }
    }

    pub async fn send(&self, message: &str) {
        self.writer_tx
            .send(message.to_string())
            .await
            .expect("Could not send message");
    }

    pub async fn recv(&mut self) -> JSONValue {
        let line = self
            .reader_rx
            .recv()
            .await
            .expect("Could not receive message");
        serde_json::from_str(&line).expect("Could not parse JSON")
    }

    pub async fn acknowledge_open(&mut self) -> JSONValue {
        let json = self.recv().await;
        if json.get("method").unwrap() == "open" {
            let id = json.get("id").unwrap();
            let response = serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": "success"
            });
            self.send(&response.to_string()).await;
            self.send("\n").await;
            // Wait a bit so that Neovim can boot up its change tracking.
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
        json
    }
}
