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
# environment. This setup is a little more strict than the default shell options
# to make sure we abort if a command in the middle of a job fails, etc.
set script-interpreter := ['bash', '-eu']
set shell := ['bash', '-eu', '-c']

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

# Verify all the things: check, lint, test, and fuzz.
[parallel]
perfect: check lint test fuzz
