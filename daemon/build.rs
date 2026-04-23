// SPDX-FileCopyrightText: 2025 EdJoPaTo <ethersync@edjopato.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: CC0-1.0

use std::fs::{create_dir_all, remove_dir_all};
use std::io::Result;
use std::path::Path;
use std::process::Command;

use clap::{CommandFactory as _, ValueEnum as _};
use clap_complete::Shell;
use clap_complete::generate_to as generate_completions_to;
use clap_mangen::generate_to as generate_manpages_to;

include!("src/cli.rs");

fn main() -> Result<()> {
    pass_on_git_version_details();
    output_completions()?;
    output_manpages()?;
    Ok(())
}

fn output_completions() -> Result<()> {
    const BIN_NAME: &str = env!("CARGO_PKG_NAME");

    let target_dir = Path::new("target");
    let compl_dir = &target_dir.join("completions");
    _ = remove_dir_all(compl_dir);
    create_dir_all(compl_dir)?;

    for &shell in Shell::value_variants() {
        generate_completions_to(shell, &mut Cli::command(), BIN_NAME, compl_dir)?;
    }
    Ok(())
}

fn output_manpages() -> Result<()> {
    let target_dir = Path::new("target");
    let man_dir = &target_dir.join("manpages");
    _ = remove_dir_all(man_dir);
    create_dir_all(man_dir)?;
    generate_manpages_to(Cli::command(), man_dir)
}

fn pass_on_git_version_details() {
    const VERSION: &str = env!("CARGO_PKG_VERSION");
    // Only add more information on pre-release versions.
    if VERSION.contains('-') {
        // Git hash
        let git_hash = Command::new("git")
            .args(["rev-parse", "--short=9", "HEAD"])
            .output()
            .ok()
            .filter(|output| output.status.success())
            .and_then(|output| String::from_utf8(output.stdout).ok())
            .map_or_else(|| "unknown".to_string(), |s| s.trim().to_string());

        // Commit date
        let commit_date = Command::new("git")
            .args(["log", "-1", "--format=%cs"])
            .output()
            .ok()
            .filter(|output| output.status.success())
            .and_then(|output| String::from_utf8(output.stdout).ok())
            .map_or_else(|| "unknown".to_string(), |s| s.trim().to_string());

        // Check if working directory is dirty
        let is_dirty = Command::new("git")
            .args(["status", "--porcelain"])
            .output()
            .ok()
            .filter(|output| output.status.success())
            .is_some_and(|output| !output.stdout.is_empty());

        println!("cargo:rustc-env=GIT_HASH={git_hash}");
        println!("cargo:rustc-env=GIT_COMMIT_DATE={commit_date}");
        println!("cargo:rustc-env=GIT_DIRTY={is_dirty}");
    }
}
