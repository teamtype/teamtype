# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

cargo := require('cargo')
cargo-deny := require('cargo-deny')
just := just_executable()
luacheck := require('luacheck')
nvim := require('nvim')
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

[group('check')]
[parallel]
check *ARGS: (check-cargo ARGS) check-typos

[group('check')]
check-cargo *ARGS:
    {{ cargo }} check {{ ARGS }}

[group('check')]
check-typos:
    {{ typos }}

[default]
[private]
@list:
    {{ just }} --list --unsorted

[group('build')]
build *ARGS:
    {{ cargo }} build {{ ARGS }}

[group('build')]
build-release: (build "--release")

[group('format')]
[parallel]
format: format-lua format-rust

[group('format')]
[working-directory("nvim-plugin")]
format-lua:
    {{ stylua }} --respect-ignores .

[group('format')]
format-rust:
    {{ cargo }} +nightly fmt

[group('lint')]
[parallel]
lint: lint-format lint-license lint-lua lint-manifests lint-rust

[group('lint')]
[parallel]
lint-format: lint-format-lua lint-format-rust

[group('lint')]
[working-directory("nvim-plugin")]
lint-format-lua:
    {{ stylua }} --respect-ignores --check .

[group('lint')]
lint-format-rust:
    {{ cargo }} +nightly fmt --check

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
    {{ cargo }} clippy

[group('test')]
test *ARGS: (test-cargo ARGS)

[group('test')]
test-cargo *ARGS:
    {{ cargo }} test {{ ARGS }}

[group('test')]
fuzz:
    {{ cargo }} test --test fuzzer

# Verify all the things: check, lint, test, and fuzz.
[parallel]
perfect: check lint test fuzz

# This task will run Neovim with factory settings plus the development version of the plug-in from this repository.
# This is especially useful for manual testing and can be used from anywhere by invoking the Justfile externally,
# e.g. with an alias such as:
#
#     alias nvim='just --justfile ~/path/to/teamtype/Justfile nvim'
#
# Run Neovim with the plug-in for testing (can be used from outside the project).
[no-cd]
nvim *ARGS:
    {{ nvim }} --clean \
        --cmd {{ quote("let &runtimepath=\"" + justfile_directory() + "/nvim-plugin,\" . &runtimepath") }} \
        --cmd 'runtime plugin/teamtype.lua' \
        {{ ARGS }}
