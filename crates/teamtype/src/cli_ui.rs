// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use console::style;
use dialoguer::Confirm;
use teamtype::traits::Interactions;

#[derive(Clone)]
pub struct ConsoleInteractions {}

impl Interactions for ConsoleInteractions {
    fn confirm(&self, question: &str) -> Result<bool> {
        Confirm::new()
            .with_prompt(question)
            .default(true)
            .show_default(true)
            .wait_for_newline(true)
            .interact()
            .context("Failed to read answer to y/n prompt")
    }

    fn inform(&self, message: &str) {
        println!("{message}");
    }

    fn error(&self, message: &str) {
        eprintln!("{}", style(message).yellow());
    }
}
