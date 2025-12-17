<!--
SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Configuration

You can put the following options into a configuration file at `.teamtype/config`:

```ini
username = <string>
peer = <secret_address>
emit_join_code = <true/false>
emit_secret_address = <true/false>
magic_wormhole_relay = <magic_wormhole_mailbox_relay_url>
```

After a successful `teamtype join`, the peer's secret address is automatically stored in your `.teamtype/config`.
In the future, you can then use `teamtype join` without a join code to reconnect to the same peer.
