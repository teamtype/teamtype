<!--
SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Ignored files

Teamtype ignores

- `.teamtype` and everything in it,
- all files ignored via `.teamtypeignore` files (which work like `.gitignore` files),
- everything that Git would [ignore](https://git-scm.com/docs/gitignore), and
- version control directories including `.git`, `.jj`, `.bzr`, `.hg`, `.pijul`, and even `.svn`, and everything in them by default.
  The `--sync-vcs` flag enables sharing these directories, see [here](git-integration-synchronized.md) for details.

Note that you shouldn't rely on `.gitignore` or `.teamtypeignore` files to "protect sensitive information" from being shared with peers you don't trust, as the peers could easily delete these ignore files.
