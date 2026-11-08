// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
// SPDX-FileCopyrightText: 2026 dommi <dommihd@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! This module is all about daemon to editor communication.

use std::env;
use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use derive_more::Deref;
use futures::StreamExt;
use tokio::io::WriteHalf;
#[cfg(windows)]
use tokio::net::windows::named_pipe::{ClientOptions, NamedPipeServer, PipeMode, ServerOptions};
#[cfg(unix)]
use tokio::net::{UnixListener, UnixStream};
use tokio_util::{
    bytes::BytesMut,
    codec::{Decoder, Encoder, FramedRead, FramedWrite, LinesCodec},
};
use tracing::debug;

use crate::daemon::{DocMessage, DocumentActorHandle};
use crate::editor_protocol::{
    EditorProtocolMessageError, IncomingMessage, JSONRPCResponse, OutgoingMessage,
};
use crate::permissions::ensure_private_dir;
use crate::sandbox;
use crate::types::UserInterface;

pub type EditorId = usize;
