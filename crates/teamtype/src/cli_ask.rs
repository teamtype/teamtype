// SPDX-FileCopyrightText: 2025 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2025 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::io::Write;
use std::io::{stdin, stdout};

use anyhow::Result;
use anyhow::bail;

pub fn ask(question: &str) -> Result<bool> {
    print!("{question} (y/N): ");
    stdout().flush()?;
    let mut lines = stdin().lines();
    if let Some(Ok(line)) = lines.next() {
        match line.to_lowercase().as_str() {
            "y" | "yes" => Ok(true),
            _ => Ok(false),
        }
    } else {
        bail!("Failed to read answer");
    }
}
