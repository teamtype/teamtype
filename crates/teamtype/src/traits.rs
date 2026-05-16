// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Result;

pub trait Interactions: Send + Sync {
    fn confirm(&self, question: &str) -> Result<bool>;
    fn inform(&self, message: &str);
}
