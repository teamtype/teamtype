// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::any::type_name;

use anyhow::Result;

/// Defines the ways Teamtype expects to be able to interact with an end user. This includes
/// printing messages through *to* the user and receiving confirmations *from* them. When using the
/// CLI interface, these interactions are provided as console functions that interact with the
/// `STDIN` and `STDOUT` streams.
///
/// Editors or editor plugins directly linking to Teamtype will need to wire these up to whatever
/// UI mechanism is available in their context.
pub trait Interactions: Send + Sync {
    /// Ask the user about some potential action or state change and receive confirmation before
    /// continuing.
    fn confirm(&self, question: &str) -> Result<bool>;

    /// Log a message that is not essential to function and may only be seen if the user has
    /// verbose mode enabled or is watching a log file, but could be useful to keep track of what is
    /// going on.
    fn log(&self, message: &str);

    /// Inform the user about an important bit of information that should be raised to their attention
    /// in whatever UI is relevant for normal operation. This may or may not interrupt a user
    /// depending on whether they look at the relevant bit of interface, but it will be presented
    /// readily available if their attention comes.
    ///
    /// These may include messages about new join codes, peer connection and disconnection notices,
    /// file actions such as remote deletions that might be unexpected, etc.
    fn inform(&self, message: &str);

    /// Raise a warning message to the users attention in the event that something went sideways and
    /// may no longer be functioning as expected.
    fn warn(&self, message: &str);

    /// Enable a blanket [`Debug`] implementation for [`crate::types::UserInterface`] by returning
    /// the name of the UI struct in use.
    fn type_name(&self) -> &'static str {
        type_name::<Self>()
    }
}
