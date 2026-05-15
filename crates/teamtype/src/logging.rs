// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;

use anyhow::Result;
use time::macros::format_description;
use tracing::level_filters::LevelFilter;
use tracing::subscriber;
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time::UtcTime};

pub fn initialize() -> Result<()> {
    let simplified_logging = env::var("RUST_LOG").is_err();

    if simplified_logging {
        let subscriber = FmtSubscriber::builder()
            .with_env_filter(EnvFilter::new("teamtype=info,fuzzer=info"))
            .without_time()
            .with_level(false)
            .with_target(false)
            .finish();
        subscriber::set_global_default(subscriber).expect("Setting default log subscriber failed");
    } else {
        let timer = UtcTime::new(format_description!("[hour]:[minute]:[second]Z"));
        let filter = EnvFilter::builder()
            .with_default_directive(LevelFilter::DEBUG.into())
            .from_env()?;
        let subscriber = FmtSubscriber::builder()
            .with_env_filter(filter)
            .with_thread_ids(true)
            .with_timer(timer)
            .finish();
        subscriber::set_global_default(subscriber).expect("Setting default log subscriber failed");
    }

    Ok(())
}
