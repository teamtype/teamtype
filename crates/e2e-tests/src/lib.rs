// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod actors;
pub mod socket;

use anyhow::Result;
use teamtype::traits::Interactions;
use tracing::{debug, info, warn};

pub struct TestInteractions {}

impl Interactions for TestInteractions {
    fn confirm(&self, question: &str) -> Result<bool> {
        debug!("Fuzzer asked for a confirmation '{question}', answering with 'false'");
        Ok(false)
    }

    fn log(&self, message: &str) {
        debug!(message);
    }

    fn inform(&self, message: &str) {
        info!(message);
    }

    fn warn(&self, message: &str) {
        warn!(message);
    }
}
