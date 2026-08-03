// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use inquire::{
    Confirm,
    ui::{RenderConfig, Styled},
};
use nu_ansi_term::{Color, Style};
use teamtype::traits::Interactions;
use tracing::debug;

#[derive(Clone)]
pub struct ConsoleInteractions {}

impl Interactions for ConsoleInteractions {
    fn confirm(&self, question: &str) -> Result<bool> {
        debug!("UI confirm event: {question}");
        Confirm::new(question)
            .with_default(false)
            .with_render_config(
                RenderConfig::default()
                    .with_prompt_separator("")
                    .with_prompt_prefix(Styled::new(""))
                    .with_answered_prompt_prefix(Styled::new("")),
            )
            .prompt()
            .context("Failed to read answer to y/n prompt")
    }

    fn log(&self, message: &str) {
        let dimmed = Style::new().dimmed();
        debug!("UI log event: {message}");
        println!("{}", dimmed.paint(message));
    }

    fn inform(&self, message: &str) {
        debug!("UI inform event: {message}");
        println!("{message}");
    }

    fn warn(&self, message: &str) {
        let yellow = Color::Yellow;
        debug!("UI warn event: {message}");
        eprintln!("{}", yellow.paint(message));
    }
}
