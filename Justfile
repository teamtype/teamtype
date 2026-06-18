# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

cargo := require('cargo')
cargo-deny := require('cargo-deny')
cargo-semver-checks := require('cargo-semver-checks')
cargo-set-version := require('cargo-set-version')
git := require('git')
git-cliff := require('git-cliff')
just := just_executable()
jq := require('jq')
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

# Block execution of other jobs if the Git working tree isn't pristine.
[group('release')]
[private]
pristine:
    # Make sure Git's status cache is warmed up.
    {{ git }} diff --shortstat
    # Ensure there are no changes in staging.
    {{ git }} diff-index --quiet --cached HEAD || exit 1
    # Ensure there are no changes in the working tree.
    {{ git }} diff-files --quiet || exit 1

read-crate-owners() := shell(cargo + ' owner --list teamtype')
read-current-user() := shell('whoami || echo $USER')
# maybe-pass(args) := if args != "" { '"$@"' } else { "" }
read-recent-tag() := shell(git + ' tag --list | tail -1')

# Verify priviledges needed for publishing, hopefully before the process is half way done.
[group('release')]
[private]
authenticated:
    # See if Git is going to allow us to push a tag by dry running an old one
    {{ git }} push --dry-run origin {{ read-recent-tag() }}
    # Verify that Cargo is logged in, can read the remote API, and that the current shell
    # user has some resemblance to listed crate owners. Not an actual proof, just a heuristic.
    [[ {{ read-crate-owners() }} =~ {{ read-current-user() }} ]]

read-manifest-version() := shell(cargo + ' metadata --no-deps --format-version 1 | ' + jq + ' -r .packages[0].version')
read-suggested-bump() := trim(shell(git-cliff, ' --unreleased --bumped-version'))

[group('release')]
[private]
validate-semver semver:
    # Is the tag even a valid semver?
    {{ semver_matches(semver, '>=' + read-manifest-version()) }}
    # Check that API changes don't suggest a different level of semver bump.
    # TODO: Remove bypass after announcing the public library API, also see https://github.com/obi1kenobi/cargo-semver-checks/pull/1652.
    {{ cargo-semver-checks }} semver-checks || true
    # Check that unreleased commit messages don't suggest a different level of semver bump.
    # TODO: Remove bypass after one release cycle of following conventional commit pattern.
    [[ {{ read-suggested-bump() }} == {{ semver }} ]] || true

read-current-branch() := shell(git, ' rev-parse --abbrev-ref HEAD')

[group('release')]
pre-release semver: pristine (validate-semver semver)
    # check that *not* on default branch
    [[ {{ default-branch }} != {{ read-current-branch() }} ]]
    # draft changelog from convntional commits
    {{ cargo-set-version }} set-version {{ semver }}
    {{ just }} perfect
    {{ cargo }} publish --dry-run --allow-dirty

# - Update the changelog
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

[group('release')]
release semver: (pre-release semver) (validate-semver semver) authenticated perfect
    {{ git }} commit -m 'chore: Release v{{ semver }}'
    {{ git }} tag v{{ semver }} -F teamtype-{{ semver }}.md
    {{ git }} push --atomic {{ default-remote }} {{ default-branch }} v{{ semver }}
    {{ cargo }} publish --locked

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

[group('release')]
post-release semver:
    # verify expected assets
    # sign?
    # - Write a Toot, or announce the release elsewhere

# - Tag the version
#     - Create a git tag v0.x.x, and push the tag
#         `git push --tags origin v0.x.x`
#     - (=> GitHub automatically builds the release and updates teamtype-nvim `main`)
#         - Quickly check that the -static binaries are created correctly, then delete this item.
