// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::PathBuf;

use anyhow::{Context, Result};

use crate::jsonrpc_forwarder::{JSONRPCForwarder, RPCForwarder};

pub async fn run_client(directory: PathBuf) -> Result<()> {
    let jsonrpc_forwarder = JSONRPCForwarder {};
    jsonrpc_forwarder
        .connection(&directory)
        .await
        .context("JSON-RPC forwarder failed")
}
