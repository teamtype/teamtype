// SPDX-FileCopyrightText: 2025 EdJoPaTo <ethersync@edjopato.de>
//
// SPDX-License-Identifier: CC0-1.0

include!("src/cli.rs");

fn main() -> std::io::Result<()> {
    use clap::{CommandFactory as _, ValueEnum as _};
    use std::process::Command;
    const BIN_NAME: &str = env!("CARGO_PKG_NAME");
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

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/cli.rs");

    let target_dir = std::path::Path::new("target");
    let compl_dir = &target_dir.join("completions");
    let man_dir = &target_dir.join("manpages");
    _ = std::fs::remove_dir_all(compl_dir);
    _ = std::fs::remove_dir_all(man_dir);
    std::fs::create_dir_all(compl_dir)?;
    std::fs::create_dir_all(man_dir)?;

    for &shell in clap_complete::Shell::value_variants() {
        clap_complete::generate_to(shell, &mut Cli::command(), BIN_NAME, compl_dir)?;
    }

    clap_mangen::generate_to(Cli::command(), man_dir)?;

    Ok(())
}
