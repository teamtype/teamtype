// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! This module is all about daemon to editor communication.
pub mod editor_unix;

use crate::daemon::DocumentActorHandle;
use crate::editor::editor_unix::EditorUnix;
use crate::editor_protocol::{
    IncomingMessage, OutgoingMessage,
};
use crate::sandbox::Sandbox;
use anyhow::Result;
use dyn_clone::DynClone;
use std::fmt::Debug;
use std::path::Path;
use tokio::io::WriteHalf;
use tokio::net::UnixStream;
use tokio_util::{
    bytes::BytesMut,
    codec::{Decoder, Encoder, FramedWrite, LinesCodec},
};

pub type EditorId = usize;

pub type EditorWriter = FramedWrite<WriteHalf<UnixStream>, OutgoingProtocolCodec>; // todo: maybe make dyn class that holds instance of this

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
    type Item = IncomingMessage;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        LinesCodec::new()
            .decode(src)?
            .map(|line| IncomingMessage::from_jsonrpc(&line))
            .transpose()
    }
}

pub trait Editor: DynClone + Debug + Send + Sync {
    fn spawn_socket_listener(&self, socket_path: &Path, document_handle: DocumentActorHandle) -> Result<()>;
    fn debug(&self){

    }
}

dyn_clone::clone_trait_object!(Editor);

pub fn new(sandbox: Box<dyn Sandbox>) -> Box<dyn Editor> {
    Box::new(EditorUnix{sandbox})
}