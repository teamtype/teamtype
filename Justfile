# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

cargo := require('cargo')
cargo-deny := require('cargo-deny')
eslint := require('eslint')
git := require('git')
git-cliff := require('git-cliff')
gh := require('gh')
jq := require('jq')
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

set default-list
set default-script
set positional-arguments
set unstable

profile := "dev"
default-remote := "origin"
default-branch := "main"

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
lint: lint-format lint-license lint-lua lint-manifests lint-rust lint-typescript

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
[working-directory("vscode-plugin")]
lint-typescript:
    {{ eslint }} --max-warnings 0 src/

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
teamtype *ARGS: build-test
    $TEAMTYPE_BINARY {{ maybe-pass(ARGS) }}

# Get an early look at what the changelog draft would look like for a release.
[group('release')]
preview-changelog:
    {{ git-cliff }} --unreleased --bump

read-last-tag() := shell(git + ' describe --tags --abbrev=0 --match="v[0-9]*" HEAD')

# Review what changes the current branch will bring to the next release's changelog draft.
[group('release')]
preview-branch-changelog:
    {{ git }} diff --no-ext-diff --no-index -- \
        <({{ git-cliff }} {{ read-last-tag() + ".." + default-remote + "/" + default-branch }}) \
        <({{ git-cliff }} --unreleased)

read-release-url(semver) := shell(gh + f" release view v{{semver}} --json url --jq .url")

# Draft a Toot announcing a release.
[group('release')]
[script]
prepare-release-toot semver:
    cat <<- EOF
    	━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    	Happy to announce the v{{ semver }} release of Teamtype! 🎉

    	Teamtype enables real-time peer-to-peer collaborative editing of local files using your own text editor.

    	Release: {{ read-release-url(semver) }}

    	Project: https://github.com/teamtype/teamtype

    	Highlights:
    	- 
    	- 
    	━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    EOF
