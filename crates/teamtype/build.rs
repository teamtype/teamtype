// SPDX-FileCopyrightText: 2025 EdJoPaTo <ethersync@edjopato.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: CC0-1.0

use std::env::var_os;
use std::fs::{create_dir_all, read, remove_dir_all, write};
use std::path::Path;
use std::process::Command;

use anyhow::Result;
use automerge::{AutoCommit, ObjType, transaction::Transactable};
use clap::{CommandFactory as _, ValueEnum as _};
use clap_complete::Shell;
use clap_complete::generate_to as generate_completions_to;
use clap_mangen::generate_to as generate_manpages_to;

include!("src/cli.rs");

fn main() -> Result<()> {
    instantiate_initial_automerge_doc()?;
    pass_on_git_version_details();
    output_completions()?;
    output_manpages()?;
    Ok(())
}

fn instantiate_initial_automerge_doc() -> Result<()> {
    let initial_automerge_doc_path = Path::new("src").join("initial_automerge_doc.bin");
    println!(
        "cargo:rerun-if-changed={}",
        &initial_automerge_doc_path.display()
    );
    let force_generate = var_os("TEAMTYPE_GENERATE_NEW_INITIAL_AUTOMERGE_DOC").is_some();
    // The initial Automerge document state is hard coded and changing it would imply a breaking
    // Teamtype release incompatible with old clients. Hence, the document generated here is tracked
    // in our VCS so we can see if it ever needs to be bumped, e.g. to use a new breaking version of
    // Automerge. Most people will never need to regenerate it, but removing the file or setting an
    // env var can be used to force a regeneration.
    if !initial_automerge_doc_path.exists() || force_generate {
        let mut automerge_doc = AutoCommit::new();
        automerge_doc.put_object(automerge::ROOT, "files", ObjType::Map)?;
        automerge_doc.put_object(automerge::ROOT, "states", ObjType::Map)?;
        let bytes = automerge_doc.save();
        write(&initial_automerge_doc_path, &bytes)?;
    }
    let automerge_doc_bytes = read(initial_automerge_doc_path)?;
    let mut initial_automerge_doc = AutoCommit::load(&automerge_doc_bytes)?;
    let doc_heads = initial_automerge_doc.get_heads();
    assert_eq!(
        doc_heads.len(),
        1,
        "Change root not found in initial Automerge document"
    );
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
    Ok(generate_manpages_to(Cli::command(), man_dir)?)
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
