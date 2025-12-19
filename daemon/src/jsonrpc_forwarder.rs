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

pub mod jsonrpc_forwarder_unix;

use async_trait::async_trait;
use std::path::Path;
use tokio_util::bytes::{Buf, BytesMut};
use tokio_util::codec::{Decoder, Encoder};

#[async_trait(?Send)]
pub trait JsonRPCForwarder {
    async fn connection(&self, socket_path: &Path) -> anyhow::Result<()>;
}

struct ContentLengthCodec;

impl Encoder<String> for ContentLengthCodec {
    type Error = std::io::Error;

    fn encode(&mut self, item: String, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let content_length = item.len();
        dst.extend_from_slice(format!("Content-Length: {content_length}\r\n\r\n").as_bytes());
        dst.extend_from_slice(item.as_bytes());
        Ok(())
    }
}

impl Decoder for ContentLengthCodec {
    type Item = String;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        // Find the position of the Content-Length header.
        let c = b"Content-Length: ";
        let Some(start_of_header) = src.windows(c.len()).position(|window| window == c) else {
            return Ok(None);
        };

        // Find the end of the line after that.
        let (end_of_line, end_of_line_bytes) = match src[start_of_header + c.len()..]
            .windows(4)
            .position(|window| window == b"\r\n\r\n")
        {
            Some(pos) => (pos, 4),
            // Even though this is not valid in terms of the spec, also
            // accept plain newline separators in order to simplify manual testing.
            None => match src[start_of_header + c.len()..]
                .windows(2)
                .position(|window| window == b"\n\n")
            {
                Some(pos) => (pos, 2),
                None => return Ok(None),
            },
        };

        // Parse the content length.
        let content_length = std::str::from_utf8(
            &src[start_of_header + c.len()..start_of_header + c.len() + end_of_line],
        )?
        .parse()?;
        let content_start = start_of_header + c.len() + end_of_line + end_of_line_bytes;

        // Recommended optimization, in anticipation for future calls to `decode`.
        src.reserve(content_start + content_length);

        // Check if we have enough content.
        if src.len() < content_start + content_length {
            return Ok(None);
        }

        // Return the body of the message.
        src.advance(content_start);
        let content = src.split_to(content_length);
        Ok(Some(std::str::from_utf8(&content)?.to_string()))
    }
}
