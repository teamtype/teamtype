// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use inquire::Confirm;
use nu_ansi_term::{Color, Style};
use teamtype::traits::Interactions;
use tracing::debug;

use crate::cli::ConsoleVerbosity;
use crate::logging::LoggingDisplay;

#[derive(Clone, Debug)]
pub struct ConsoleInteractions {
    pub verbosity: ConsoleVerbosity,
}

impl From<ConsoleVerbosity> for LoggingDisplay {
    fn from(value: ConsoleVerbosity) -> Self {
        match value {
            ConsoleVerbosity::Quiet => Self::Compact,
            ConsoleVerbosity::Verbose => Self::Pretty,
        }
    }
}

impl Interactions for ConsoleInteractions {
    fn confirm(&self, question: &str) -> Result<bool> {
        debug!("UI confirm event: {question}");
        Confirm::new(question)
            .with_default(false)
            .prompt()
            .context("Failed to read answer to y/n prompt")
    }

    fn log(&self, message: &str) {
        debug!("UI log event: {message}");
        if matches!(self.verbosity, ConsoleVerbosity::Verbose) {
            let dimmed = Style::new().dimmed();
            println!("{}", dimmed.paint(message));
        }
    }

    fn inform(&self, message: &str) {
        debug!("UI inform event: {message}");
        println!("{message}");
    }

    fn warn(&self, message: &str) {
        debug!("UI warn event: {message}");
        let yellow = Color::Yellow;
        eprintln!("{}", yellow.paint(message));
    }
}
