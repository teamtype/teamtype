// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

#![doc = include_str!("../README.md")]

// Public modules, either used by CLI or exported via crate or bindings
pub mod client;
pub mod config;
pub mod daemon;
pub mod logging;
pub mod sandbox;
pub mod traits;
pub mod types;

// Used by e2e test crate, but not officially public
#[doc(hidden)]
pub mod document;
#[doc(hidden)]
pub mod editor_protocol;

// Private modules
mod editor;
mod editor_connection;
mod jsonrpc_forwarder;
mod ot;
mod path;
mod peer;
mod watcher;
mod wormhole;
