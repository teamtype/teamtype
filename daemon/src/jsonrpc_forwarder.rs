// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! Provides a way to write / read a socket through stdin, (un)packing content-length encoding.
//!
//! The idea is that a daemon process communicates through newline separated jsonrpc messages,
//! whereas LSP expects an HTTP-like Base Protocol:
//! <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol>
//!
//! This forwarder thus
//! - takes jsonrpc from a socket (usually a daemon) and wraps it content-length encoded data to stdout
//! - takes content-length encoded data from stdin (as sent by an LSP client) and writes it
//!   "unpacked" to the socket

use std::path::Path;

use async_trait::async_trait;
use futures::{SinkExt, StreamExt};
use teamtype::types::ContentLengthCodec;
use tokio::io::{AsyncRead, AsyncWrite, BufReader, BufWriter};
use tokio::net::UnixStream;
use tokio::net::unix::{OwnedReadHalf, OwnedWriteHalf};
use tokio_util::codec::{FramedRead, FramedWrite, LinesCodec};

// TODO: Put these defaults to a module accessible by config.rs as well.
pub const DEFAULT_SOCKET_NAME: &str = "socket";
pub const CONFIG_DIR: &str = ".teamtype";

#[async_trait(?Send)]
pub trait JSONRPCForwarder<
    R: AsyncRead + Unpin + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
>
{
    async fn connect_stream(
        &self,
        directory: &Path,
    ) -> anyhow::Result<(FramedRead<R, LinesCodec>, FramedWrite<W, LinesCodec>)>;

    async fn connection(&self, base_dir: &Path) -> anyhow::Result<()> {
        let (mut socket_read, mut socket_write) = self.connect_stream(base_dir).await?;

        // Construct stdin/stdout objects, which send/receive messages with a Content-Length header.
        let mut stdin = FramedRead::new(BufReader::new(tokio::io::stdin()), ContentLengthCodec);
        let mut stdout = FramedWrite::new(BufWriter::new(tokio::io::stdout()), ContentLengthCodec);

        tokio::spawn(async move {
            while let Some(Ok(message)) = socket_read.next().await {
                stdout
                    .send(message)
                    .await
                    .expect("Failed to write to stdout");
            }
            // Socket was closed.
            std::process::exit(0);
        });

        while let Some(Ok(message)) = stdin.next().await {
            socket_write.send(message).await?;
        }
        // Stdin was closed.
        std::process::exit(0);
    }
}

pub struct UnixJSONRPCForwarder {}

#[async_trait(?Send)]
impl JSONRPCForwarder<OwnedReadHalf, OwnedWriteHalf> for UnixJSONRPCForwarder {
    async fn connect_stream(
        &self,
        directory: &Path,
    ) -> anyhow::Result<(
        FramedRead<OwnedReadHalf, LinesCodec>,
        FramedWrite<OwnedWriteHalf, LinesCodec>,
    )> {
        // Construct socket object, which send/receive newline-delimited messages.
        let socket_path = directory.join(CONFIG_DIR).join(DEFAULT_SOCKET_NAME);
        let stream = UnixStream::connect(socket_path).await?;
        let (socket_read, socket_write) = stream.into_split();
        let socket_read = FramedRead::new(socket_read, LinesCodec::new());
        let socket_write = FramedWrite::new(socket_write, LinesCodec::new());
        Ok((socket_read, socket_write))
    }
}
