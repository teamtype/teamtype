<!--
SPDX-FileCopyrightText: 2026 blinry <mail@blinry.org>
SPDX-FileCopyrightText: 2026 zormit <nt4u@kpvn.de>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Running your own relays

By default, Teamtype uses a couple of public relays hosted by other people/groups.
They facilitate smooth connections between devices in different local networks.
All transmitted data is always end-to-end encrypted between peers.
After a connection is established, peers connect directly, if possible.

If you don't want to rely on these public relays, you can host your own ones, and instruct Teamtype to use them.
Here's how that works for each of the relays.
Note that we mostly describe a "development testing setup" here; for a production deployment, please look into the individual relays more carefully.

## Magic Wormhole relay

Teamtype uses a [Magic Wormhole relay server](https://github.com/magic-wormhole/magic-wormhole-mailbox-server) (also known as "mailbox server" or "rendezvous server") to share the initial join codes with other peers.

### Running your own

Install [uv](https://docs.astral.sh/uv/), then run:

    uvx --from twisted --with magic-wormhole-mailbox-server twist wormhole-mailbox

This will start the relay on port 4000 by default.

### Using it

Run

    teamtype share --magic-wormhole-relay ws://localhost:4000/v1

and

    teamtype join --magic-wormhole-relay ws://localhost:4000/v1 <join code>

or set the `magic_wormhole_relay` parameter in `.teamtype/config`.

## Iroh Pkarr relay + DNS server

Teamtype uses Iroh's [Pkarr relay and DNS server](https://github.com/n0-computer/iroh/tree/main/iroh-dns-server) to allow peers to meet on the same Iroh relay (see below).
Read [Iroh's discovery documentation](https://docs.iroh.computer/concepts/discovery) for more information.

### Running your own

Using Rust's `cargo`, run `cargo install iroh-dns-server@0.35.0`.
Then, run:

    iroh-dns-server

This will start a Pkarr relay on port 8080, and a DNS server on port 5300 by default, which resolves subdomains of `irohdns.example`.
You can provide configuration files for production setups, see the [README of iroh-dns-server](https://github.com/n0-computer/iroh/tree/main/iroh-dns-server).

### DNS testing setup

For a development setup, you need to configure your computer to use your local DNS server.
Assuming you're on Linux and use `systemd-resolved`, run

    sudo resolvectl dns <interface> 127.0.0.1:5300

to set sets it as a DNS server for a network interface, run `resolvectl status` for your options.
Then, run

    sudo resolvectl domain <interface> '~irohdns.example'

so that only domains under `irohdns.example` are resolved using your local server.

### Using it

Run

    teamtype share --iroh-pkarr-relay http://localhost:8080/pkarr

and

    teamtype join --iroh-dns-domain irohdns.example <join code>

or set the `iroh_pkarr_relay` and `iroh_dns_domain` parameters in `.teamtype/config`.

It doesn't hurt to use both flags on both the sharing and the joining peer.

## Iroh relay

Teamtype uses an [Iroh relay](https://github.com/n0-computer/iroh/tree/main/iroh-relay) when direct connections between peers aren't immediately possible.
Read [Iroh's relay documentation](https://docs.iroh.computer/concepts/relays) for more information.

### Running your own

Using Rust's `cargo`, run `cargo install cargo install iroh-relay@0.35.0 --features=server`.
Then, run:

    iroh-relay --dev

This will start the relay on port 3340 by default.

### Using it

Run

    teamtype share --iroh-relay http://localhost:3340

and

    teamtype join --iroh-relay http://localhost:3340 <join code>

or set the `iroh_relay` parameter in `.teamtype/config`.
