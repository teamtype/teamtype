# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

cargo := require('cargo')
cargo-deny := require('cargo-deny')
just := just_executable()
luacheck := require('luacheck')
nix := require('nix')
nvim := require('nvim')
prettier := require('prettier')
reuse := require('reuse')
stylua := require('stylua')
typos := require('typos')

export TEAMTYPE_BINARY := justfile_directory() + "/target/debug/teamtype"

# By default Just will re-use the user's $SHELL. In order to make use of script
# rules and more advanced shell features we need a more predictable runtime
# environment. This setup is a little more strict than the default shell options
# to make sure we abort if a command in the middle of a job fails, etc.
set script-interpreter := ['bash', '-eu']
set shell := ['bash', '-eu', '-c']

set positional-arguments
set unstable

profile := "dev"

# With positional arguments enabled, we can pass all the arguments to the bash
# shell in a way that will get expanded to the original 'word' breakdown. However,
# when we do this blindly in all cases and the job's positional arguments happen
# to be empty the shell decides we must have wanted a placeholder for an empty
# string argument — a construct that is invalid for many of our commands. The
# solution is to decide up front whether we have any positional arguments at all
# and then either not pass anything or pass them in a way that will get expanded
# properly. As a caveat we can't use this workaround for nested jobs that pass
# positional arguments to other jobs since one layer of quoting is lost, but we
# don't need to because none of those happen to use spaces in arguments anyway.
maybe-pass(args) := if args != "" { '"$@"' } else { "" }

[group('check')]
[parallel]
check *ARGS: (check-cargo ARGS) check-typos

[group('check')]
check-cargo *ARGS:
    {{ cargo }} check --all-targets --all-features {{ ARGS }}

[group('check')]
check-typos:
    {{ typos }}

[default]
[private]
@list:
    {{ just }} --list --unsorted

[group('build')]
build *ARGS:
    {{ cargo }} build --profile {{ profile }} {{ ARGS }}

[group('build')]
build-release *ARGS:
    {{ just }} --set profile release build {{ ARGS }}

[group('build')]
build-test *ARGS:
    {{ just }} --set profile test build {{ ARGS }}

[group('format')]
[parallel]
format: format-lua format-nix format-rust format-typescript

[group('format')]
[working-directory("nvim-plugin")]
format-lua:
    {{ stylua }} --respect-ignores .

[group('format')]
format-nix:
    {{ nix }} fmt flake.nix

[group('format')]
format-rust:
    {{ cargo }} +nightly fmt

[group('format')]
format-typescript:
    {{ prettier }} --write **.ts

[group('lint')]
[parallel]
lint: lint-format lint-license lint-lua lint-manifests lint-rust

[group('lint')]
[parallel]
lint-format: lint-format-lua lint-format-rust lint-format-typescript

[group('lint')]
[working-directory("nvim-plugin")]
lint-format-lua:
    {{ stylua }} --respect-ignores --check .

[group('lint')]
lint-format-rust:
    {{ cargo }} +nightly fmt --check

[group('lint')]
lint-format-typescript:
    {{ prettier }} --check **.ts

[group('lint')]
lint-license:
    {{ reuse }} lint

[group('lint')]
[working-directory("nvim-plugin")]
lint-lua:
    {{ luacheck }} .

[group('lint')]
lint-manifests:
    {{ cargo-deny }} check

[group('lint')]
lint-rust:
    {{ cargo }} clippy --all-targets --all-features

[group('test')]
test *ARGS: (test-cargo ARGS)

[group('test')]
test-cargo *ARGS: build
    {{ cargo }} test {{ ARGS }}

[group('test')]
fuzz: build
    {{ cargo }} test --test fuzzer

# Verify all the things: check, lint, test, and fuzz.
[parallel]
perfect: check lint test fuzz

# This task will run Neovim with factory settings but wired to the development version of the client from this repository.
# This is especially useful for manual testing and can be used from anywhere by invoking the Justfile externally,
# e.g. with an alias such as:
#
#     alias nvim='just --justfile ~/path/to/teamtype/Justfile nvim'
#
# Run Neovim with the plug-in for testing (can be used from outside the project).
[no-cd]
[script]
nvim *ARGS: build-test
    {{ nvim }} --clean \
        --cmd {{ quote("let &runtimepath=\"" + justfile_directory() + "/nvim-plugin,\" . &runtimepath") }} \
        --cmd 'runtime plugin/teamtype.lua' \
        {{ maybe-pass(ARGS) }}

# This task will build (if necessary) and run the Teamtype CLI via the development version from this repository.
# This is especially useful for manual testing and can be used from anywhere by invoking the Justfile externally,
# e.g. with an alias such as:
#
#     alias teamtype='just --justfile ~/path/to/teamtype/Justfile teamtype'
#
# Build and run Teamtype for testing (can be used from outside the project).
[no-cd]
[script]
teamtype *ARGS: build-test
    $TEAMTYPE_BINARY {{ maybe-pass(ARGS) }}

pre-release semver:
	# 

release semver:

post-release semver:

# - Check the changelog wiki page for breaking changes or TODOs
#     - *could* be determined using conventional commit messages, by an !
# - Update the changelog
#     - changelog generator could release a first draft of it
#         - caleb likes git-cliff
#         - caleb usually filters out tooling changes
#             - one PR could have many commits, but only 1-2 feat commits
#         - could potentially also contain the body, and link to the PR
#         - maybe preface it with a summary
#         - plan: have job generate a "first draft", ready for editing
#         - prerelease
#             - generate changelog draft
#             - check tests
#         - release
#         - postrelease (to check for existence of binaries in release?)
#     - `git log v0.3.0..HEAD --reverse --oneline`
#     - Change the heading that says (unreleased) to the actual release heading, including the date
#     - Create a commit
# - Bump the Cargo TOML version
#     - Remove dev
#         - caleb's suggestion: don't use -dev, but add build.rs to add -r<num> to version
#     - Run `cargo check` in daemon/ and in daemon/integration-tests to bump the teamtype dependency
#     - Create commit
# - Create a new branch (release-0.x.y)
#     - Open a PR with it (so CI can run)
# - Publish to Cargo
#     - (optional) cargo publish --dry-run
#     - cargo publish
# - If necessary, re-release the VS Code plugin
#     - (follow the release steps in vscode-plugin/DEVELOPMENT.md)
#     - Create commit
#     - Generate a token for vsce publish, e.g., at https://dev.azure.com/nt4u/_usersSettings/tokens
#         - Token not working? Create a new one. A “personal” one for yourself is sufficient.
# - Tag the version
#     - Merge the PR
#     - Create a git tag v0.x.x, and push the tag
#         `git push --tags origin v0.x.x`
#     - (=> GitHub automatically builds the release and updates teamtype-nvim `main`)
#         - Quickly check that the -static binaries are created correctly, then delete this item.
# - Next dev release number
#     - Add a new heading in the changelog, marked (unreleased)
#     - In Cargo.toml
# - Write a Toot, or announce the release elsewhere
#
# Pre-releases?
#
# - Crates can have a pre-release part: https://doc.rust-lang.org/cargo/reference/manifest.html#the-version-field
#     - e.g., 0.7.0-beta.0, then 0.7.0-beta.1, etc.
# - Neovim currently has no versions; it follows the main branch
#     - In the future: releases only on the main branch? And then a development branch for more recent versions?
# - VS Code https://code.visualstudio.com/api/working-with-extensions/publishing-extension#prerelease-extensions
#     - No support for pre-release tags :/ “Full semver support will be available in the future.”
#     - Recommendation: major.EVEN_NUMBER.patch for release versions, major.ODD_NUMBER.patch for pre-releases...
#         - I guess we should probably do it that way for now...
#         - Alternative: 0.major.(minor+1), and then possibly 0.(major+1).0
#         - Or just go straight to 0.(major+1).0. And the first “stable” version would then be 0.(major+1).1
#     - Use the vsce package --pre-release!
