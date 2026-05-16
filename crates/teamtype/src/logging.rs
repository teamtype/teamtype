// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;

use anyhow::Result;
use time::macros::format_description;
use tracing::info;
use tracing::level_filters::LevelFilter;
use tracing::subscriber;
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time::UtcTime};

pub fn initialize(verbose: bool) -> Result<()> {
    let default_level = if verbose { "info" } else { "warn" };
    if env::var("RUST_LOG").is_err() {
        let subscriber = FmtSubscriber::builder()
            .with_env_filter(EnvFilter::new(format!("teamtype={default_level}")))
            .without_time()
            .with_target(false)
            .with_level(true)
            .compact()
            .finish();
        subscriber::set_global_default(subscriber).expect("Setting default log subscriber failed");
        info!(
            "Initialized CLI with verbose output. For more detail use `RUST_LOG=debug teamtype` or similar"
        );
    } else {
        let timer = UtcTime::new(format_description!("[hour]:[minute]:[second]Z"));
        let filter = EnvFilter::builder()
            .with_default_directive(LevelFilter::DEBUG.into())
            .from_env()?;
        let subscriber = FmtSubscriber::builder()
            .with_env_filter(filter)
            .with_thread_ids(true)
            .with_timer(timer)
            .with_level(true)
            .pretty()
            .finish();
        subscriber::set_global_default(subscriber).expect("Setting default log subscriber failed");
    }

    Ok(())
}
