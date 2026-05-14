# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

cargo := require('cargo')
cargo-deny := require('cargo-deny')
just := just_executable()
luacheck := require('luacheck')
reuse := require('reuse')
stylua := require('stylua')
typos := require('typos')

# By default Just will re-use the user's $SHELL. In order to make use of script
# rules and more advanced shell features we need a more predictable runtime
# environment. ZSH just happens to be @alerque's favorite. This setup is a little
# more strict than the default shell options to make sure we abort if a command
# fails, doesn't accept empty glob expansions, etc.
set script-interpreter := ['zsh', '+o', 'nomatch', '-eu']
set shell := ['zsh', '+o', 'nomatch', '-ecu']

set positional-arguments
set unstable

[parallel]
check: check-cargo check-typos

check-cargo:
    {{ cargo }} check

check-typos:
    {{ typos }}

[default]
[private]
@list:
    {{ just }} --list --unsorted

build *ARGS:
    {{ cargo }} build {{ ARGS }}

build-release: (build "--release")

[parallel]
format: format-lua format-rust

[working-directory("nvim-plugin")]
format-lua:
    {{ stylua }} --respect-ignores .

format-rust:
    {{ cargo }} +nightly fmt

[parallel]
lint: lint-format lint-license lint-lua lint-manifests lint-rust

[parallel]
lint-format: lint-format-lua lint-format-rust

[working-directory("nvim-plugin")]
lint-format-lua:
    {{ stylua }} --respect-ignores --check .

lint-format-rust:
    {{ cargo }} +nightly fmt --check

lint-license:
    {{ reuse }} lint

[working-directory("nvim-plugin")]
lint-lua:
    {{ luacheck }} .

lint-manifests:
    {{ cargo-deny }} check

lint-rust:
    {{ cargo }} clippy

test: test-cargo

test-cargo:
    {{ cargo }} test

fuzz:
    {{ cargo }} test --test fuzzer

[parallel]
perfect: check lint test fuzz

# - Check the changelog wiki page for breaking changes or TODOs
# - Update the changelog
#     - `git log v0.3.0..HEAD --reverse --oneline`
#     - Change the heading that says (unreleased) to the actual release heading, including the date
#     - Create a commit
# - Bump the Cargo TOML version
#     - Remove dev
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
