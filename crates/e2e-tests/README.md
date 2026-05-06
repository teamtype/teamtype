<!--
SPDX-FileCopyrightText: 2024 Danny McClanahan <dmcC2@hypnicjerk.ai>
SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

This directory contains end-to-end integration tests for Teamtype.
They assume that your `PATH` contains:

- An `nvim` with installed Teamtype plugin, and
- a `teamtype` binary (for connecting via `teamtype client`).

To run all e2e tests except the fuzzer, run:

```bash
cargo test
```

To run specifically the fuzzer, run:

```bash
cargo test --profile e2e --test fuzzer
```
