// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;
use std::fmt;

use anyhow::Result;
use nu_ansi_term::{Color, Style};
use time::macros::format_description;
use tracing::info;
use tracing::level_filters::LevelFilter;
use tracing::subscriber;
use tracing::{Event, Level, Subscriber};
use tracing_subscriber::fmt::{
    FmtContext,
    format::{FormatEvent, FormatFields, Writer},
};
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time::UtcTime};

pub fn initialize(verbose: bool) -> Result<()> {
    let default_level = if verbose { "info" } else { "warn" };
    if env::var("RUST_LOG").is_err() {
        let subscriber = FmtSubscriber::builder()
            .with_env_filter(EnvFilter::new(format!("teamtype={default_level}")))
            .with_ansi(true) // Let the rest of the builder understand we are colorized.
            .event_format(ColoredLine)
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

/// Modified clone of `Compact` formatter from tracing-subscriber. Eschews the optional level
/// labels, time formatting, and target options for hard coded defaults and uses the same ANSI
/// handling internals to colorize the whole line instead of only the level label.
struct ColoredLine;

impl<S, N> FormatEvent<S, N> for ColoredLine
where
    S: Subscriber + for<'a> LookupSpan<'a>,
    N: for<'a> FormatFields<'a> + 'static,
{
    fn format_event(
        &self,
        ctx: &FmtContext<'_, S, N>,
        mut writer: Writer<'_>,
        event: &Event<'_>,
    ) -> fmt::Result {
        let mut buf = String::new();
        let buf_writer = Writer::new(&mut buf);
        // We told the event builder we're using ANSI escapes here, but we are about to wrap the
        // output of the existing `Compact` formatter and we do *not* want it to colorize stuff
        // for us. We'll do that by wrapping the whole buffered output.
        let fmt = tracing_subscriber::fmt::format()
            .compact()
            .with_ansi(false)
            .without_time()
            .with_target(false)
            .with_level(false);
        fmt.format_event(ctx, buf_writer, event)?;
        // We want the ANSI coloring to be inside the trailing newline of each message.
        let line = buf.trim_end_matches('\n');
        let style = if writer.has_ansi_escapes() {
            match *event.metadata().level() {
                Level::ERROR => Color::Red.normal(),
                Level::WARN => Color::Yellow.normal(),
                // The typical color for info logs would be `Green`, but that is overbearing when
                // colorizing the whole line, so just render info messages in the terminal default.
                Level::INFO => Style::default(),
                Level::DEBUG => Color::Blue.normal(),
                Level::TRACE => Color::Purple.normal(),
            }
        } else {
            return writeln!(writer, "{line}");
        };
        write!(writer, "{}", style.paint(line))?;
        writeln!(writer)
    }
}
