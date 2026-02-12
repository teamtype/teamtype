use std::path::Path;
use async_trait::async_trait;
use futures::{SinkExt, StreamExt};
use tokio::io::{BufReader, BufWriter};
use tokio::net::UnixStream;
use tokio_util::codec::{FramedRead, FramedWrite, LinesCodec};
use crate::jsonrpc_forwarder::{ContentLengthCodec, JsonRPCForwarder};

pub struct UnixJsonRPCForwarder {}

#[async_trait(?Send)]
impl JsonRPCForwarder for UnixJsonRPCForwarder {
    async fn connection(&self, socket_path: &Path) -> anyhow::Result<()> {
        // Construct socket object, which send/receive newline-delimited messages.
        let stream = UnixStream::connect(socket_path).await?;
        let (socket_read, socket_write) = stream.into_split();
        let mut socket_read = FramedRead::new(socket_read, LinesCodec::new());
        let mut socket_write = FramedWrite::new(socket_write, LinesCodec::new());

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