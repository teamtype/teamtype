// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Result;

/// Defines the ways Teamtype expects to be able to interact with an end user.
///
/// Editors or editor plugins directly linking to Teamtype will need to wire these up to whatever
/// UI mechanism is available in their context.
pub trait UserInteraction: Send + Sync {
    /// Ask the user about some potential action or state change and receive confirmation before
    /// continuing.
    fn confirm(&self, question: &str) -> Result<bool>;
}
