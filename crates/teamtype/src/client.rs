// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};

use crate::config::Config;
use crate::jsonrpc_forwarder::{JSONRPCForwarder, UnixJSONRPCForwarder};

pub async fn run_client(config: Config) -> Result<()> {
    let base_dir = config
        .base_dir
        .context("Client mode cannot create a new temporary directory.")?;
    let jsonrpc_forwarder = UnixJSONRPCForwarder {};
    jsonrpc_forwarder
        .connection(&base_dir)
        .await
        .context("JSON-RPC forwarder failed")
}
