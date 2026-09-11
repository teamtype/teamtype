// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;

use anyhow::{Context, Result};
use time::macros::format_description;
use tracing::info;
use tracing::subscriber;
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time::UtcTime};

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoggingDisplay {
    #[default]
    Pretty,
    Compact,
}

pub fn initialize(display: LoggingDisplay) -> Result<()> {
    if env::var("RUST_LOG").is_ok() {
        let filter = EnvFilter::builder().from_env()?;

        let builder = FmtSubscriber::builder()
            .with_env_filter(filter)
            .with_level(true)
            .with_ansi(true);

        let subscriber: Box<dyn tracing::Subscriber + Send + Sync> = match display {
            LoggingDisplay::Compact => {
                info!(
                    "Initialized log output in compact mode, remove quiet flag for pretty mode. Set the level with `env RUST_LOG=teamtype=debug teamtype ...` or similar."
                );
                Box::new(builder.compact().finish())
            }
            LoggingDisplay::Pretty => {
                info!(
                    "Initialized log output in pretty mode, use quiet flag for compact mode. Set the level with `env RUST_LOG=teamtype=debug teamtype ...` or similar."
                );
                let timer = UtcTime::new(format_description!("[hour]:[minute]:[second]Z"));
                Box::new(
                    builder
                        .with_thread_ids(true)
                        .with_timer(timer)
                        .pretty()
                        .finish(),
                )
            }
        };

        subscriber::set_global_default(subscriber)
            .context("Setting default log subscriber failed")?;
    }
    Ok(())
}
