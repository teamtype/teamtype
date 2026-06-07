// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};

use crate::config::Config;
use crate::jsonrpc_forwarder::{JSONRPCForwarder, UnixJSONRPCForwarder};

pub async fn run_client(config: Config) -> Result<()> {
    let jsonrpc_forwarder = UnixJSONRPCForwarder {};
    jsonrpc_forwarder
        .connection(&config.base_dir)
        .await
        .context("JSON-RPC forwarder failed")
}
