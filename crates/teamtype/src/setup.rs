// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::{Path, PathBuf};

use anyhow::bail;
use anyhow::{Context, Result};
use docstr::docstr;
use microxdg::XdgApp;

use crate::config::CONFIG_DIR;
use crate::config::ProjectDir;
use crate::permissions::ensure_private_dir;
use crate::sandbox;
use crate::types::UserInterface;
