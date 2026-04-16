<!--
SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

<img src="https://raw.githubusercontent.com/teamtype/teamtype-design/refs/heads/main/logo/regular/on-light.svg" width="250px">

*Multiplayer mode for your text editor!*

For complete end-user documentation see the [Teamtype Documentation][docs] site.

> [!NOTE]
> Until October 2025, this project was known as "Ethersync".
> The motivation for the rename was explained in [this PR][formerly].

Teamtype enables real-time peer-to-peer collaborative editing of local files using your own text editor or other tools.
For example, you can use it for pair programming sessions, brainstorming, or note-taking.
It's the missing real-time complement Git and other snapshot based version control systems!

![Demo video for how to make a connection and of collaborating in Neovim][demo-video]

## Features

- 👥 Edit files concurrently by multiple users using different text editors
- 📍 See your peers' cursors, selections, and edits in real time
- 🗃️ Collaborate on entire directories, not just single files
- 🔒 Rest easy with encrypted peer-to-peer connections, no need for a server
- ✒️ Maintain full access with a local-first model that retains all content on disk even if you go offline
- 🧩 Write new editor plugins using a [simple JSON-RPC protocol]][dev-guide]

We also maintain a list of [other collaborative text-editing software][related-projects].

## What Teamtype is *not*

We are not a company and do not have anything to sell.
We do not require you to create an account.
We do not have access to your data, and do not use it to train AI algorithms.
We do not serve you ads or track you in any way.

We are just a bunch of people building something we want to use ourselves.

## 🚦 Project status

We're currently maintaining Teamtype in our free time.
We're often using it for pair programming ourselves, but there's still some [bugs][bugs] to be aware of.

## 📥 Installation

### 1. Install the `teamtype` command

Teamtype works on Linux, macOS, Android, and on the Windows Subsystem for Linux.

<details>
<summary>Binary releases</summary>

The [releases on GitHub][releases] come with precompiled static binaries.
Download one and put it somewhere in your shell's [`PATH`][path-variable]:

- `x86_64-unknown-linux-musl` for Linux
- `universal-apple-darwin` for macOS
- `aarch64-unknown-linux-musl` for Android (use a terminal emulator like [Termux][termux])

</details>

<details>
<summary>Arch Linux</summary>

```bash
sudo pacman -S teamtype
```
</details>

<details>
<summary>Homebrew</summary>

```bash
brew install teamtype
```
</details>

<details>
<summary>Nix</summary>

To put `teamtype` in your PATH temporarily, run:

```bash
nix shell nixpkgs#teamtype
```

Make sure to also have it in your PATH when you run the editors, or install it to your environment in your preferred way.
</details>

<details>
<summary>Via Cargo</summary>

```bash
cargo install --locked teamtype
```

To save some compilation time, you can use [cargo-binstall][cargo-binstall], which will install a precompiled binary attached to our latest release:

```bash
cargo binstall teamtype
```

</details>

### 2. Install an editor plugin

- [Neovim][teamtype-nvim]
- VS Code/Codium: Install the "Teamtype" extension from the marketplace

## 📖 Basic usage

In the directory you want to share:

```console
$ teamtype share

    To connect to you, another person can run:

    teamtype join 5-hamburger-endorse

Peer connected: adfa90edd932732ddf242f24dc2dcd6156779e69966d432fcb3b9fe3ae9831ab
```

Another person, in a separate directory (also works on the same computer):

```console
$ teamtype join 5-hamburger-endorse

Derived peer from join code. Storing in config (overwriting previous config).
Storing peer's address in .teamtype/config.
Connected to peer: 5e6b787fff79074735eb9b56939269100de1e37bc7f7a4d29c277cc24f7ee53d
```

The directories are now connected, and changes will be synced instantly.
You can open text files (using editors with a Teamtype plugin), and start collaborating in real time! :sparkles:

## 🎓 Learn more

- Learn more about Teamtype in [the documentation][docs].
- Watch a [10-minute talk][fosdem-2025] given at FOSDEM 2025.
- Watch a (German) [1-hour talk][mrmcd-2024] given at MRMCD 2024.

## 🏘️ Community projects

Plugins:

- @schrieveslaach is maintaining a [plugin for Jetbrains IDEs][teamtype-jetbrains]
- [TeXstudio][texstudio] has a [Teamtype integration][texstudio-integration]

Work-in-progress plugins:

- @sohalt's [Emacs plugin](https://github.com/sohalt/ethersync.el)
- @hvergara's [Obsidian plugin](https://github.com/critica-tech-lab/pasta-obsidian)
- @winniehell's [web editor](https://github.com/teamtype/teamtype-web)
- [Emacs plugin](https://github.com/teamtype/teamtype/tree/emacs-plugin)
- @thwischm's [Kakoune plugin](https://github.com/thwischm/kak-ethersync)

Bridges:

- [Hedgedoc bridge prototype](https://github.com/teamtype/teamtype/tree/hedgedoc-prototype/daemon)
- @dglittle's [Braid bridge](https://github.com/braid-org/braid-ethersync)
- [LSP bridge](https://github.com/teamtype/teamtype-lsp)
- @3timeslazy's [LSP bridge](https://github.com/nonscalable/teamtype-lsp/tree/main/crates/teamtype-lsp) (see this [discussion](https://github.com/teamtype/teamtype/discussions/440))

## 🔨 Contributing

We'd love to receive your patches and other contributions!
Small patches are very welcome as PRs.
Before starting to implement a new big feature, please briefly [check in with us](#contact) so we can discuss how it fits in with our ideas for the project.
We have a [CONTRIBUTING.md](CONTRIBUTING.md) file that gives more guidance.
Please take note of our [**strict no-LLM policy**](CONTRIBUTING.md).

If you're interested in building new editor plugins, read the [editor plugin development guide][dev-guide].
For more information about Teamtype's design, refer to the list of [decision records](docs/decisions/).

If you find bugs, please [open an issue][issues] on Github!

## ☎️ Contact

Feel free to [join us on Zulip][zulip] to ask us anything!
Other good channels:

- Mastodon: [@teamtype@fosstodon.org][mastodon]
- Email: <span>t<span title="ihate@spam.com&lt;/span&gt;">e</span>amtype</span>@zormit<i title="&lt;/i&gt;mailto:">.</i>de

## 💚 Thanks

Teamtype received funding from [NLNet][nlnet]'s [NGI0 Core Fund][ngio-core] throughout 2024.

Thanks to the [Prototype Fund][prototype-fund] and the [Federal Ministry of Research, Technology and Space][bmbf] for funding this project in 2025.

<a href="https://nlnet.nl/"><img src="https://upload.wikimedia.org/wikipedia/en/a/a4/NLnet_Foundation_logo.svg" alt="Logo of the NLnet Foundation" style="height: 70px;"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://prototypefund.de/en/"><img src="https://upload.wikimedia.org/wikipedia/commons/b/ba/Prototype_Fund_Logo_2025.svg" alt="Logo of the Prototype Fund" style="height: 70px;"></a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://okfn.de/en/"><img src="https://upload.wikimedia.org/wikipedia/commons/4/4d/Open_Knowledge_Foundation_Deutschland_Logo.svg" alt="Logo of the Open Knowledge Foundation Germany" style="height: 100px;"></a>
&nbsp;&nbsp;
<a href="https://www.bmbf.de/EN/"><img src="https://upload.wikimedia.org/wikipedia/commons/d/df/BMFTR_Logo.svg" alt="Logo of the German Federal Ministry of Research, Technology and Space" style="height: 110px;"></a>

Teamtype is based on [Automerge][automerge], [Iroh][iroh], and [Magic Wormhole][magic-wormhole].

And finally, thanks to everyone who helped us beta-test, or reported issues!

## 📜 License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This project is [REUSE][reuse] compliant, see the headers of each file for licensing information.

[automerge]: https://automerge.org
[bmbf]: https://www.bmbf.de/EN/
[bugs]: https://github.com/teamtype/teamtype/issues?q=sort%3Aupdated-desc+is%3Aissue+is%3Aopen+%28label%3Abug+OR+type%3ABug%29
[cargo-binstall]: https://github.com/cargo-bins/cargo-binstall
[demo-video]: https://files.blinry.org/teamtype-share-join-demo.gif
[dev-guide]: https://teamtype.github.io/teamtype/editor-plugin-dev-guide.html
[docs]: https://teamtype.github.io/teamtype
[formerly]: https://github.com/teamtype/teamtype/pull/436
[fosdem-2025]: https://fosdem.org/2025/schedule/event/fosdem-2025-4890-ethersync-real-time-collaboration-in-your-text-editor-/
[iroh]: https://www.iroh.computer
[issues]: https://github.com/teamtype/teamtype/issues
[magic-wormhole]: https://magic-wormhole.readthedocs.io
[mastodon]: https://fosstodon.org/@teamtype
[mrmcd-2024]: https://media.ccc.de/v/2024-355-ethersync-echtzeit-kollaboration-in-deinem-texteditor-
[ngio-core]: https://nlnet.nl/core/
[nlnet]: https://nlnet.nl
[path-variable]: https://en.wikipedia.org/wiki/PATH_(variable)
[prototype-fund]: https://www.prototypefund.de/
[related-projects]: https://teamtype.github.io/teamtype/related-projects.html
[releases]: https://github.com/teamtype/teamtype/releases/latest
[reuse]: https://reuse.software
[teamtype-jetbrains]: https://github.com/teamtype/teamtype-jetbrains
[teamtype-nvim]: (https://github.com/teamtype/teamtype-nvim)
[termux]: https://termux.dev
[texstudio-integration]: https://texstudio-org.github.io/editing.html#collaborative-editing-pair-programming
[texstudio]: https://www.texstudio.org
[zulip]: https://teamtype.zulipchat.com
