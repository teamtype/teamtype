// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
// SPDX-FileCopyrightText: 2026 dommi <dommihd@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! This module is all about daemon to editor communication.

#[cfg(unix)]
use std::env;
use std::path::Path;
#[cfg(unix)]
use std::path::PathBuf;

use anyhow::bail;
use anyhow::{Context, Result};
use futures::StreamExt;
use tokio::io::WriteHalf;
#[cfg(windows)]
use tokio::net::windows::named_pipe::{NamedPipeServer, PipeMode, ServerOptions};
#[cfg(unix)]
use tokio::net::{UnixListener, UnixStream};
use tokio_util::{
    bytes::BytesMut,
    codec::{Decoder, Encoder, FramedRead, FramedWrite, LinesCodec},
};
use tracing::{debug, error, info};

use crate::daemon::{DocMessage, DocumentActorHandle};
use crate::editor_protocol::{
    EditorProtocolMessageError, IncomingMessage, JSONRPCResponse, OutgoingMessage,
};
use crate::permissions::check_mode;
use crate::sandbox;
use crate::types::UserInterface;

pub type EditorId = usize;

#[cfg(unix)]
pub type EditorWriter = FramedWrite<WriteHalf<UnixStream>, OutgoingProtocolCodec>;
#[cfg(windows)]
pub type EditorWriter = FramedWrite<WriteHalf<NamedPipeServer>, OutgoingProtocolCodec>;

#[cfg(unix)]
pub type EditorStream = UnixStream;
#[cfg(windows)]
pub type EditorStream = NamedPipeServer;

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

#[cfg(unix)]
pub fn strip_current_dir(path: &Path) -> PathBuf {
    let Ok(cwd) = env::current_dir() else {
        return path.to_path_buf();
    };
    path.strip_prefix(&cwd)
        .map_or_else(|_| path.to_path_buf(), Path::to_path_buf)
}

/// # Panics
///
/// Will panic if we fail to setup a listener using a socket (Unix) or named pipe (Windows), or if
/// we fail to accept an incoming connection.
pub fn spawn_listener(
    listener_path: &Path,
    document_handle: DocumentActorHandle,
    ui: &UserInterface,
) -> Result<()> {
    let listener_dir = listener_path
        .parent()
        .context("Invalid socket creation location")?;
    // Make sure the parent directory of the socket is only accessible by the current user.
    check_mode(listener_dir, 0o77700u32)?;

    // Using the sandbox method here is technically unnecessary, but we want to really run all path
    // operations through the sandbox module.
    if sandbox::exists(
        listener_dir.parent().context("Presumed base_dir invalid")?,
        listener_path,
    )
    .expect("Failed to check existence of path")
    {
        let listener_path_display = listener_path.display();
        let remove_listener = ui.confirm(&format!(
            "Detected an existing listener '{listener_path_display}'. There might be a daemon running already for this directory, or the previous one crashed. Do you want to continue?"
        ));
        if remove_listener? {
            sandbox::remove_file(Path::new("/"), listener_path).expect("Could not remove listener");
        } else {
            bail!("Not continuing, make sure to stop all other daemons on this directory");
        }
    }

    #[cfg(unix)]
    {
        // The std library function used to create sockets requires a path shorter than SUN_LEN, but the
        // length that matters is only the segment it is asked to handle. If passed an absolute path
        // here we can potentially be run in a path that exceeds the maximum (~100 chars). Passing it a
        // relative path effectively sidesteps this limitation. Stripping the leading path segments will
        // result in a relative path that won't have a long cumbersome prefix that fails safety checks.
        // The extra song and dance to change into the parent directory first is not needed by our CLI
        // (which already changes to that location) but it will make this API usable when linked as a
        // library without changing the parent thread's location for keeps.
        let previous_cwd = env::current_dir()?;
        env::set_current_dir(listener_dir)?;
        let listener = UnixListener::bind(strip_current_dir(listener_path))?;
        env::set_current_dir(previous_cwd)?;
        debug!("Listening on UNIX socket: {}", listener_path.display());

        tokio::spawn(async move {
            loop {
                match listener.accept().await {
                    Ok((stream, _addr)) => {
                        let id = document_handle.clone().next_editor_id();
                        let document_handle_clone = document_handle.clone();
                        tokio::spawn(async move {
                            handle_editor_connection(stream, document_handle_clone.clone(), id)
                                .await;
                        })
                    }
                    Err(err) => {
                        panic!("Error while accepting socket connection: {err}");
                    }
                };
            }
        });
    }

    #[cfg(windows)]
    {
        let pipe_name = format!(
            r"\\.\pipe\{}",
            listener_path.to_str().unwrap().split('\\').last().unwrap()
        );

        tokio::spawn(async move {
            loop {
                let mut server_options = ServerOptions::new();
                server_options.pipe_mode(PipeMode::Byte);
                // Only allow local connections.
                server_options.reject_remote_clients(true);
                // TODO: only allow current user to connect => custom security_descriptor => server_options.create_with_security_attributes_raw()
                let listener: NamedPipeServer = server_options.create(&pipe_name).unwrap();
                debug!("Listening for connections on named pipe: {}", pipe_name);
                match listener.connect().await {
                    Ok(()) => {
                        let id = document_handle.clone().next_editor_id();
                        let document_handle_clone = document_handle.clone();
                        tokio::spawn(async move {
                            handle_editor_connection(listener, document_handle_clone.clone(), id)
                                .await;
                        });
                    }
                    Err(err) => {
                        panic!("Error while accepting named pipe connection: {err}");
                    }
                };
            }
        });
    }

    Ok(())
}

async fn handle_editor_connection(
    stream: EditorStream,
    document_handle: DocumentActorHandle,
    editor_id: EditorId,
) {
    let (stream_read, stream_write) = tokio::io::split(stream);
    let mut reader = FramedRead::new(stream_read, IncomingProtocolCodec);
    let writer = FramedWrite::new(stream_write, OutgoingProtocolCodec);

    document_handle
        .send_message(DocMessage::NewEditorConnection(editor_id, writer))
        .await;
    info!("Editor #{editor_id} connected.");

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
                error!("Error for JSON-RPC request: {:?}", response);
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
    info!("Editor #{editor_id} disconnected.");
}
