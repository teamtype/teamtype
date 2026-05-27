// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
// SPDX-FileCopyrightText: 2026 dommi <dommihd@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! This module is all about daemon to editor communication.

use std::env;
use std::os::unix::net::UnixStream as StdUnixStream;
use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use futures::StreamExt;
use tokio::{
    io::WriteHalf,
    net::{UnixListener, UnixStream},
};
use tokio_util::{
    bytes::BytesMut,
    codec::{Decoder, Encoder, FramedRead, FramedWrite, LinesCodec},
};
use tracing::debug;

use crate::daemon::{DocMessage, DocumentActorHandle};
use crate::editor_protocol::{
    EditorProtocolMessageError, IncomingMessage, JSONRPCResponse, OutgoingMessage,
};
use crate::permissions::check_mode;
use crate::sandbox;
use crate::types::UserInterface;

pub type EditorId = usize;

pub type EditorWriter = FramedWrite<WriteHalf<UnixStream>, OutgoingProtocolCodec>;

#[derive(Debug)]
pub struct OutgoingProtocolCodec;

impl Encoder<OutgoingMessage> for OutgoingProtocolCodec {
    type Error = anyhow::Error;

    fn encode(&mut self, item: OutgoingMessage, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let payload = item.to_jsonrpc()?;
        dst.extend_from_slice(format!("{payload}\n").as_bytes());
        Ok(())
    }
}

#[derive(Debug)]
pub struct IncomingProtocolCodec;

impl Decoder for IncomingProtocolCodec {
    type Error = anyhow::Error;
    type Item = IncomingMessage;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        LinesCodec::new()
            .decode(src)?
            .map(|line| IncomingMessage::from_jsonrpc(&line))
            .transpose()
    }
}

pub fn strip_current_dir(path: &Path) -> PathBuf {
    let Ok(cwd) = env::current_dir() else {
        return path.to_path_buf();
    };
    path.strip_prefix(&cwd)
        .map_or_else(|_| path.to_path_buf(), Path::to_path_buf)
}

/// # Panics
///
/// Will panic if we fail to listen on the socket, or if we fail to accept an incoming connection.
pub fn spawn_listener(
    listener_path: &Path,
    document_handle: DocumentActorHandle,
    ui: &UserInterface,
) -> Result<()> {
    let parent_path = listener_path
        .parent()
        .context("Invalid socket creation location")?;
    // Make sure the parent directory of the socket is only accessible by the current user.
    check_mode(parent_path, 0o77700u32)?;

    // Using the sandbox method here is technically unnecessary, but we want to really run all path
    // operations through the sandbox module.
    // TODO: Use correct directory as guard.
    if sandbox::exists(Path::new("/"), Path::new(&listener_path))
        .expect("Failed to check existence of path")
    {
        // If there's an existing socket, try to connect to it as a client. If that fails, we assume
        // there's no other daemon running and we can delete the socket.
        if StdUnixStream::connect(strip_current_dir(listener_path)).is_ok() {
            bail!(
                "Detected an existing daemon running for this directory. Rejecting to start another one."
            );
        }
        ui.warn(
            "An existing socket was found for this directory, but since the daemon seems to be defunct it is being removed."
        );
        sandbox::remove_file(Path::new("/"), listener_path).unwrap_or_else(|_| {
            panic!(
                "Could not remove existing daemon's listener '{}'",
                listener_path.display()
            )
        });
    }

    // The std library function used to create sockets requires a path shorter than SUN_LEN, but the
    // length that matters is only the segment it is asked to handle. If passed an absolute path
    // here we can potentially be run in a path that exceeds the maximum (~100 chars). Passing it a
    // relative path effectively sidesteps this limitation. Stripping the leading path segments will
    // result in a relative path that won't have a long cumbersome prefix that fails safety checks.
    // The extra song and dance to change into the parent directory first is not needed by our CLI
    // (which already changes to that location) but it will make this API usable when linked as a
    // library without changing the parent thread's location for keeps.
    let previous_cwd = env::current_dir()?;
    env::set_current_dir(parent_path)?;
    let listener = UnixListener::bind(strip_current_dir(listener_path))?;
    env::set_current_dir(previous_cwd)?;
    debug!("Listening on UNIX socket: {}", listener_path.display());

    tokio::spawn({
        let ui = ui.clone();
        async move {
            loop {
                match listener.accept().await {
                    Ok((stream, _addr)) => {
                        let id = document_handle.clone().next_editor_id();
                        let document_handle_clone = document_handle.clone();
                        tokio::spawn({
                            let ui = ui.clone();
                            async move {
                                handle_editor_connection(
                                    stream,
                                    document_handle_clone.clone(),
                                    id,
                                    &ui,
                                )
                                .await;
                            }
                        })
                    }
                    Err(err) => {
                        panic!("Error while accepting socket connection: {err}");
                    }
                };
            }
        }
    });

    Ok(())
}

async fn handle_editor_connection(
    stream: UnixStream,
    document_handle: DocumentActorHandle,
    editor_id: EditorId,
    ui: &UserInterface,
) {
    let (stream_read, stream_write) = tokio::io::split(stream);
    let mut reader = FramedRead::new(stream_read, IncomingProtocolCodec);
    let writer = FramedWrite::new(stream_write, OutgoingProtocolCodec);

    document_handle
        .send_message(DocMessage::NewEditorConnection(editor_id, writer))
        .await;
    ui.log(&format!("Editor #{editor_id} connected."));

    while let Some(message) = reader.next().await {
        match message {
            Ok(message) => {
                document_handle
                    .send_message(DocMessage::FromEditor(editor_id, message))
                    .await;
            }
            Err(e) => {
                let response = JSONRPCResponse::RequestError {
                    id: None,
                    error: EditorProtocolMessageError {
                        code: -32700,
                        message: format!("Invalid request: {e}"),
                        data: None,
                    },
                };
                ui.warn(&format!("Error for JSON-RPC request: {response:?}"));
                let message = OutgoingMessage::Response(response);
                document_handle
                    .send_message(DocMessage::ToEditor(editor_id, message))
                    .await;
            }
        }
    }
    // Err(e) => {
    // }

    document_handle
        .send_message(DocMessage::CloseEditorConnection(editor_id))
        .await;
    ui.log(&format!("Editor #{editor_id} disconnected."));
}
