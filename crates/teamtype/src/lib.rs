// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

#![doc = include_str!("../README.md")]

// Organize code internally via private modules
mod client;
mod config;
mod daemon;
mod editor;
mod editor_connection;
mod jsonrpc_forwarder;
mod ot;
mod path;
mod peer;
mod permissions;
mod traits;
mod watcher;
mod wormhole;

// The e2e testing reaches deep into internals of these modules that we don't advertise as part of
// our supported public API, but they still need to cross a crate boundary.
//
// These should eventually all be refactored so that anything that needs deep internal access is
// implemented with unit tests, and the e2e testing only uses public APIs. Until then hide these
// from documented exports so nobody else is tempted to use them.
#[doc(hidden)]
pub mod document;
#[doc(hidden)]
pub mod editor_protocol;
#[doc(hidden)]
pub mod sandbox;
#[doc(hidden)]
pub mod setup;
#[doc(hidden)]
pub mod types;

#[doc(hidden)]
pub use daemon::TEST_FILE_PATH;

// Used by unit tests and hence compiled in a different crate context, but not public.
#[cfg(test)]
pub(crate) mod testing;

// Explicitly export bits from our modules that we have picked to be the public API.
pub use client::run_client;
pub use config::{ProjectDir, Config, NetworkMode, Peer, VcsMode};
pub use daemon::Daemon;
pub use daemon::run_daemon;
pub use traits::Interactions;
pub use types::UserInterface;
