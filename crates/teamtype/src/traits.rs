// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Result;

pub trait UserInteraction {
    fn confirm(&self, question: &str) -> Result<bool>;
}
