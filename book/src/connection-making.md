<!--
SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Connection making

## Join codes

When you run `teamtype share`, you will get a short "join code" like `3-exhausted-bananas`.
Another person can use it to connect to you!
The code only works once.
You can learn about the security properties in the [Magic Wormhole documentation](https://magic-wormhole.readthedocs.io/en/latest/welcome.html#safely).

<div class="mermaid">
sequenceDiagram
participant A as teamtype share
participant W as Magic Wormhole relay
participant B as teamtype join
A->>W: join code
A->>B: join code (via some secure channel)
B->>W: join code
rect rgb(191,233,255)
Note over A,B: Magic Wormhole establishes end-to-end encrypted channel
A-->>B: secret address
end
</div>

## Secret addresses

Since version 0.7.0 Teamtype uses iroh for making a connection.
To connect to another daemon, we're using a combination of the iroh [Node Identifier](https://www.iroh.computer/docs/concepts/endpoint#node-identifiers) and a secret key which, smashed together, which looks like `429e94...0e9819#32374e...4a6789`.
We call this the node's *secret address*.
Treat it like a password.
After using a join code, the secret address is stored in your `.teamtype/config`.

## Peer to peer

You can directly connect across different local networks, even when each of you is behind a router.
This way of connecting is more "ad hoc" and useful if you want to collaborate over a short period of time (as described in more detail in the [pair programming scenario](pair-programming.md)).

<div class="mermaid">
sequenceDiagram
participant A as teamtype share
participant Ip as Iroh Pkarr relay
participant Id as Iroh DNS server
participant Ir as Iroh Relay
participant B as teamtype join
A->>Ir: my Node Identifier + connection information
A->>Ip: my Node Identifier + relay
Ip->>Id: Publishes on
B->>Id: peer's Node Identifier
Id->>B: peer's relay
B->>Ir: peer's Node Identifier
Ir->>B: connection information
rect rgb(191,233,255)
note over A,B: Establish an end-to-end encrypted channel, either directly, or via the relay
B-->>A: secret key
A->>A: Confirms secret key
A<<-->>B: Sync messages
end
</div>

Teamtype also uses a "local discovery" mechanism based on mDNS, which allows peers in the same local network to find each other.

<div class="mermaid">
sequenceDiagram
participant A as teamtype share
participant B as teamtype join
A->>B: occasionally broadcasts own Node Identifer + connection information
B->>A: direct connection
</div>

## Cloud peer

When you want to have an "always online" host, such that every user can connect to it at the time of their liking, let's say you're collaborating in a group on [taking notes](shared-notes.md).

Other systems solve this with a client-server architecture, where the server is always online, and the clients connect to it as needed.

But Teamtype is fundamentally peer-to-peer, so what we suggest to use is what the research group Ink & Switch call a ["cloud peer"](https://www.inkandswitch.com/local-first/): You run a Teamtype peer on a public server, and all users will then connect to that server.

This is only recommended for people who are comfortable setting up services on a server.
But the nice part is that if someone did this for you, you can just connect to it not worrying about the nitty-gritty networking details.
