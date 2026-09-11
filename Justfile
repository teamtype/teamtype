# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

cargo := require('cargo')
cargo-deny := require('cargo-deny')
cargo-semver-checks := require('cargo-semver-checks')
cargo-set-version := require('cargo-set-version')
eslint := require('eslint')
gh := require('gh')
git := require('git')
git-cliff := require('git-cliff')
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

read-current-user() := replace(shell('whoami || echo $USER'), "mn", "zormit")
read-recent-tag() := shell(git + ' tag --list | tail -1')

# Verify privileges needed for publishing, hopefully before the process is half way done.
[group('release')]
[private]
check-credentials:
    # See if Git is going to allow us to push a tag by dry running an old one
    {{ git }} push --dry-run origin {{ read-recent-tag() }}
    # Verify that Cargo is logged in, can read the remote API, and that the current shell
    # user has some resemblance to listed crate owners. Not an actual proof, just a heuristic.
    {{ cargo }} owner --list teamtype | grep -qF '{{ read-current-user() }}'

read-manifest-version() := shell(cargo + ' metadata --no-deps --format-version 1 | ' + jq + ' -r .packages[0].version')
read-suggested-bump() := trim(shell(git-cliff + ' --unreleased --bumped-version'))

# Run smoke tests on a proposed semver, check that it's a big enough bump to satisfy tooling's urges.
[group('release')]
[private]
validate-semver semver:
    # Is the tag even a valid semver?
    {{ semver_matches(semver, '>=' + read-manifest-version()) }}
    # Check that API changes don't suggest a different level of semver bump.
    # TODO: Remove bypass after announcing the public library API, also see https://github.com/obi1kenobi/cargo-semver-checks/pull/1652.
    {{ cargo-semver-checks }} semver-checks || true
    # Check that unreleased commit messages don't suggest a different level of semver bump.
    [[ {{ read-suggested-bump() }} == {{ semver }} ]]

rev-parse(ref) := shell(git + ' rev-parse --abbrev-ref ' + ref)

# - Create a new branch (release-0.x.y)
#     - Open a PR with it (so CI can run)

# Open a PR with a proposed change log to be edited into release notes.
[group('release')]
prepare-release semver: pristine (validate-semver semver) perfect
    # check that *not* on default branch
    [[ {{ default-branch }} != {{ rev-parse('HEAD') }} ]]
    # draft changelog from conventional commits
    {{ just }} perfect
    {{ cargo }} publish --dry-run --allow-dirty

# - Create a new branch (release-0.x.y)
#     - Open a PR with it (so CI can run)

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

release-pr-filter(semver) := f'.[] | select(.headRepository.nameWithOwner == "teamtype/teamtype") | select(.headRefName == "release-v{{semver}}") | .number'
find-release-pr(semver) := shell(gh + f" pr list --json headRefName,headRepository,number --jq '{{release-pr-filter(semver)}}'")
read-pr-merge-state(prno) := shell(gh + f" pr view {{prno}} --json mergeStateStatus --jq .mergeStateStatus")

# Gate release action on having previously and properly prepared one.
[group('release')]
[private]
release-is-prepared semver:
    # Check that changelog draft PR is ready for merge.
    [[ "{{ read-pr-merge-state(find-release-pr(semver)) }}" == "CLEAN" ]]

# Set the version in manifests, tag, commit, and publish.
[group('release')]
release semver: (release-is-prepared semver) check-credentials perfect
    # consider auto-merging release PR?
    {{ cargo-set-version }} set-version {{ semver }}
    # stage changes
    {{ git }} commit -m 'chore: Release v{{ semver }}'
    # extract changelog blob
    # add release header to changelog blog
    # TODO: Sign release tags
    {{ git }} tag v{{ semver }} -F teamtype-{{ semver }}.md
    {{ cargo }} publish --dry-run
    {{ git }} push --atomic {{ default-remote }} {{ default-branch }} v{{ semver }}
    {{ cargo }} publish --locked

count-release-assets(semver) := shell(gh + ' release view v' + semver + ' --json assets --jq ".assets | length"')
read-release-url(semver) := shell(gh + f" release view v{{semver}} --json url --jq .url")

# Verify everything is copastetic and suggest next steps.
[group('release')]
post-release semver:
    # Verify release has some metadata.
    {{ gh }} release view v{{ semver }} | sed '/^--$/q'
    # Expect exactly 3 assets attached to completed releases.
    [[ {{ count-release-assets(semver) }} -eq 3 ]]
    # TODO: add asset attestation
    {{ gh }} release verify v{{ semver }} || true
    # TODO: mark release as immutable
    {{ just }} prepare-release-toot {{ semver }}
    echo {{ read-release-url(semver) }}

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
