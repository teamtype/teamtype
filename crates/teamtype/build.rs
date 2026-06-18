// SPDX-FileCopyrightText: 2025 EdJoPaTo <ethersync@edjopato.de>
// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: CC0-1.0

use std::env::{var, var_os};
use std::fs::{create_dir_all, read, remove_dir_all, write};
use std::path::Path;

use anyhow::Result;
use automerge::{AutoCommit, ObjType, transaction::Transactable};
use clap::{CommandFactory as _, ValueEnum as _};
use clap_complete::Shell;
use clap_complete::generate_to as generate_completions_to;
use clap_mangen::generate_to as generate_manpages_to;
use regex::Regex;
use vergen_git2::{Emitter, Git2Builder};

include!("src/cli.rs");

fn main() -> Result<()> {
    instantiate_initial_automerge_doc()?;
    let version = derive_version_details()?;
    // Pass version info for use in CLI's --version flag.
    println!("cargo::rustc-env=TEAMTYPE_VERSION={version}");
    // Skip generating extra artifacts if being run via `cargo install --git ...`.
    println!("cargo::rerun-if-env-changed=CARGO_WORKSPACE_DIR");
    if var_os("CARGO_WORKSPACE_DIR").is_some() {
        output_completions(version)?;
        output_manpages(version)?;
    }
    Ok(())
}

fn instantiate_initial_automerge_doc() -> Result<()> {
    let initial_automerge_doc_path = Path::new("src").join("initial_automerge_doc.bin");
    println!(
        "cargo::rerun-if-changed={}",
        initial_automerge_doc_path.display()
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

fn output_completions(version: &'static str) -> Result<()> {
    const BIN_NAME: &str = env!("CARGO_PKG_NAME");
    let workspace_root = PathBuf::from(var("CARGO_WORKSPACE_DIR")?);
    let target_dir = &workspace_root.join("target");
    let compl_dir = &target_dir.join("completions");
    _ = remove_dir_all(compl_dir);
    create_dir_all(compl_dir)?;
    for &shell in Shell::value_variants() {
        generate_completions_to(
            shell,
            &mut Cli::command().version(version),
            BIN_NAME,
            compl_dir,
        )?;
    }
    Ok(())
}

fn output_manpages(version: &'static str) -> Result<()> {
    let workspace_root = PathBuf::from(var("CARGO_WORKSPACE_DIR")?);
    let target_dir = &workspace_root.join("target");
    let man_dir = &target_dir.join("manpages");
    _ = remove_dir_all(man_dir);
    create_dir_all(man_dir)?;
    Ok(generate_manpages_to(
        Cli::command().version(version),
        man_dir,
    )?)
}

fn derive_version_details() -> Result<&'static str> {
    if var("VERGEN_GIT_SHA").is_err() {
        (|| -> Result<()> {
            let git2 = Git2Builder::all_git()?;
            Emitter::default().add_instructions(&git2)?.emit_and_set()?;
            Ok(())
        })()
        .map_err(|e| {
            anyhow::anyhow!(
                "Could not determine git version info: {e}\n\
                 Either build from a git checkout, or set VERGEN_GIT_SHA, \
                 VERGEN_GIT_COMMIT_DATE, VERGEN_GIT_DIRTY, and VERGEN_GIT_DESCRIBE."
            )
        })?;
    }
    let version = formulate_version()?;
    Ok(version)
}

// Take special environment variables passed by Cargo during the build and bundle them up into our
// desired version string format in a way that Clap will digest at build time in a derive macro.
fn formulate_version() -> Result<&'static str> {
    let manifest_version = env!("CARGO_PKG_VERSION").to_string();
    let git_sha = var("VERGEN_GIT_SHA")?;
    let commit_date = var("VERGEN_GIT_COMMIT_DATE")?;
    let is_dirty = var("VERGEN_GIT_DIRTY")?;
    // The `git describe` handler only picks up *annotated* tags, which through 0.9.1 were not made.
    let git_describe = var("VERGEN_GIT_DESCRIBE")?;
    let describe_expression =
        Regex::new(r"^v(?<tag>\d+\.\d+\.\d+)-(?<counter>\d+)-g(?<sha>[0-9a-f]+)$")?;
    let semver = if let Some(captures) = describe_expression.captures(&git_describe) {
        if manifest_version != captures["tag"] {
            println!(
                "cargo::warning=The most recent annotated tag '{}' does not match the manifest version '{}'.",
                &captures["tag"], &manifest_version
            );
        }
        // The `git describe` used by vergen will not output the counter on a tag anyway, but our
        // reconstruction of the same data in the Nix flake might.
        if &captures["counter"] == "0" {
            captures["tag"].to_string()
        } else {
            format!("{}+r{}", &captures["tag"], &captures["counter"])
        }
    } else {
        manifest_version
    };
    let dirty_suffix = matches!(is_dirty.as_ref(), "true")
        .then_some("-dirty")
        .unwrap_or("");
    let git_sha_prefix = git_sha.get(..9).unwrap_or(&git_sha);
    let version = format!("{semver} ({git_sha_prefix}{dirty_suffix} {commit_date})");
    Ok(Box::leak(version.into_boxed_str()))
}
