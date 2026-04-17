// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 TNG Technology Consulting GmbH <christoph.niehoff@tngtech.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

//! This module provides a [`ConnectionManager`], which can be used to connect to other daemons.

use self::sync::{Connection, PeerMessage, SyncActor};
use crate::daemon::DocumentActorHandle;
use anyhow::{Context, Result, bail};
use async_trait::async_trait;
use iroh::discovery::{
    ConcurrentDiscovery,
    dns::DnsDiscovery,
    pkarr::{PkarrPublisher, PkarrResolver},
};
use iroh::endpoint::{RecvStream, RelayMode, SendStream};
use iroh::{NodeAddr, RelayMap, RelayUrl, SecretKey};
use postcard::{from_bytes, to_allocvec};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;
use tokio::sync::{mpsc, oneshot};
use tokio::time::sleep;
use tracing::{debug, info, warn};

mod sync;

const ALPN: &[u8] = b"/teamtype/0";

struct SecretAddress {
    node_addr: NodeAddr,
    passphrase: SecretKey,
}

impl FromStr for SecretAddress {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.split('#').collect();
        if parts.len() != 2 {
            bail!("Peer string must have format <node_id>#<passphrase>");
        }

        let node_addr = iroh::PublicKey::from_str(parts[0])?.into();
        let passphrase = SecretKey::from_str(parts[1])?;

        Ok(Self {
            node_addr,
            passphrase,
        })
    }
}

enum PeerAuth {
    MyPassphrase(SecretKey),
    YourPassphrase(SecretKey),
}

pub struct ConnectionManager {
    message_tx: mpsc::Sender<EndpointMessage>,
    secret_address: String,
}

impl ConnectionManager {
    pub async fn new(
        document_handle: DocumentActorHandle,
        base_dir: &Path,
        relay: Option<String>,
        discovery: Option<String>,
    ) -> Result<Self> {
        let (message_tx, message_rx) = mpsc::channel(1);

        let (endpoint, my_passphrase) = Self::build_endpoint(base_dir, relay, discovery).await?;

        let secret_address = format!("{}#{}", endpoint.node_id(), my_passphrase);

        let mut actor = EndpointActor::new(
            endpoint,
            message_rx,
            message_tx.clone(),
            document_handle,
            my_passphrase,
        );

        tokio::spawn(async move { actor.run().await });

        Ok(Self {
            message_tx,
            secret_address,
        })
    }

    #[must_use]
    pub fn secret_address(&self) -> &str {
        &self.secret_address
    }

    pub async fn connect(&self, secret_address: String) -> Result<()> {
        let (response_tx, response_rx) = oneshot::channel();

        self.message_tx
            .send(EndpointMessage::Connect {
                secret_address: SecretAddress::from_str(&secret_address)?,
                response_tx: Some(response_tx),
                previous_attempts: 0,
            })
            .await
            .expect("EndpointActor task has been killed");

        response_rx.await??;

        Ok(())
    }

    async fn build_endpoint(
        base_dir: &Path,
        relay: Option<String>,
        discovery: Option<String>,
    ) -> Result<(iroh::Endpoint, SecretKey)> {
        let (secret_key, my_passphrase) = Self::get_keypair(base_dir);

        let mut builder = iroh::Endpoint::builder()
            .secret_key(secret_key.clone())
            .alpns(vec![ALPN.to_vec()]);

        match (&relay, &discovery) {
            (None, None) => {
                // Default: use n0's infrastructure (current behavior).
                builder = builder.discovery_n0();
            }
            _ => {
                // Custom: configure relay and discovery separately.
                if let Some(relay_url) = &relay {
                    let url: RelayUrl = relay_url.parse()?;
                    let relay_map = RelayMap::from(url);
                    builder = builder.relay_mode(RelayMode::Custom(relay_map));
                    info!("Using custom iroh relay: {}", relay_url);
                }

                if let Some(discovery_url) = &discovery {
                    let pkarr_url: url::Url = discovery_url.parse()?;
                    let concurrent = ConcurrentDiscovery::from_services(vec![
                        Box::new(PkarrPublisher::new(secret_key.clone(), pkarr_url.clone())),
                        Box::new(PkarrResolver::new(pkarr_url)),
                    ]);
                    builder = builder.discovery(Box::new(concurrent));
                    info!("Using custom pkarr discovery: {}", discovery_url);
                } else {
                    // relay is set but discovery is not: keep n0's discovery services.
                    builder = builder
                        .add_discovery(|sk| Some(PkarrPublisher::n0_dns(sk.clone())))
                        .add_discovery(|_| Some(PkarrResolver::n0_dns()))
                        .add_discovery(|_| Some(DnsDiscovery::n0_dns()));
                }
            }
        }

        let endpoint = builder.bind().await?;
        Ok((endpoint, my_passphrase))
    }

    fn get_keypair(base_dir: &Path) -> (SecretKey, SecretKey) {
        let keyfile = base_dir.join(".teamtype").join("key");
        if keyfile.exists() {
            let metadata =
                fs::metadata(&keyfile).expect("Expected to have access to metadata of the keyfile");

            let current_permissions = metadata.permissions().mode();
            let allowed_permissions = 0o100_600;
            assert!(
                current_permissions == allowed_permissions,
                "For security reasons, please make sure to set the key file to user-readable only (set the permissions to 600)."
            );

            assert!(
                metadata.len() == 64,
                "Your keyfile is not 64 bytes long. This is a sign that it was created by a Teamtype version older than 0.7.0, which is not compatible. Please remove .teamtype/key, and try again."
            );

            debug!("Re-using existing keypair.");
            let mut file = File::open(keyfile).expect("Failed to open key file");

            let mut secret_key = [0; 32];
            file.read_exact(&mut secret_key)
                .expect("Failed to read from key file");

            let mut passphrase = [0; 32];
            file.read_exact(&mut passphrase)
                .expect("Failed to read from key file");

            (
                SecretKey::from_bytes(&secret_key),
                SecretKey::from_bytes(&passphrase),
            )
        } else {
            debug!("Generating new keypair.");
            let secret_key = SecretKey::generate(rand::rngs::OsRng);
            let passphrase = SecretKey::generate(rand::rngs::OsRng);

            let mut file = OpenOptions::new()
                .create_new(true)
                .write(true)
                .mode(0o600)
                .open(keyfile)
                .expect("Should have been able to create key file that did not exist before");

            file.write_all(&secret_key.to_bytes())
                .expect("Failed to write to key file");
            file.write_all(&passphrase.to_bytes())
                .expect("Failed to write to key file");

            (secret_key, passphrase)
        }
    }
}

enum EndpointMessage {
    // Instruct the endpoint to connect to a new peer.
    Connect {
        // All information we need to connect to another peer.
        secret_address: SecretAddress,
        // On connection success, this channel will be pinged.
        // Used for the initial connection, where we want to fail if connecting fails.
        response_tx: Option<oneshot::Sender<Result<()>>>,
        // How many times have we already attempted to connect?
        previous_attempts: usize,
    },
}

// Owns the Iroh endpoint, accepts incoming connections, and can be instructed to connect to
// another daemon.
struct EndpointActor {
    endpoint: iroh::Endpoint,
    message_rx: mpsc::Receiver<EndpointMessage>,
    message_tx: mpsc::Sender<EndpointMessage>,
    document_handle: DocumentActorHandle,
    my_passphrase: SecretKey,
}

impl EndpointActor {
    fn new(
        endpoint: iroh::Endpoint,
        message_rx: mpsc::Receiver<EndpointMessage>,
        message_tx: mpsc::Sender<EndpointMessage>,
        document_handle: DocumentActorHandle,
        my_passphrase: SecretKey,
    ) -> Self {
        Self {
            endpoint,
            message_rx,
            message_tx,
            document_handle,
            my_passphrase,
        }
    }

    async fn handle_message(&self, message: EndpointMessage) -> Result<()> {
        match message {
            EndpointMessage::Connect {
                secret_address,
                response_tx,
                previous_attempts,
            } => {
                let node_addr = secret_address.node_addr.clone();
                let connect_result = self.endpoint.connect(node_addr, ALPN).await;
                let conn = match connect_result {
                    Ok(conn) => conn,
                    Err(err) => {
                        if let Some(response_tx) = response_tx {
                            response_tx
                                .send(Err(err))
                                .expect("Connect receiver dropped");
                        }
                        Self::reconnect(self.message_tx.clone(), secret_address, previous_attempts)
                            .await
                            .expect("Failed to initiate reconnection");
                        // Not really Ok, but Ok enough.
                        return Ok(());
                    }
                };

                info!(
                    "Connected to peer: {}",
                    conn.remote_node_id()
                        .expect("Connection should have a node ID")
                );

                if let Some(response_tx) = response_tx {
                    response_tx.send(Ok(())).expect("Connect receiver dropped");
                }

                let document_handle_clone = self.document_handle.clone();
                let message_tx_clone = self.message_tx.clone();
                tokio::spawn(async move {
                    if let Err(err) = Self::handle_peer(
                        document_handle_clone,
                        conn,
                        PeerAuth::YourPassphrase(secret_address.passphrase.clone()),
                    )
                    .await
                    {
                        debug!("Error while handling a peer: {:?}", err);
                    }
                    Self::reconnect(message_tx_clone, secret_address, 0)
                        .await
                        .expect("Failed to initiate reconnection");
                });
            }
        }
        Ok(())
    }

    async fn reconnect(
        message_tx: mpsc::Sender<EndpointMessage>,
        secret_address: SecretAddress,
        previous_attempts: usize,
    ) -> Result<()> {
        // Only log at "info" level if this is the first reconnection attempt.
        if previous_attempts == 0 {
            info!(
                "Connection to peer {} lost, will keep trying to reconnect...",
                secret_address.node_addr.node_id
            );
        } else {
            sleep(Duration::from_secs(10)).await;
            debug!(
                "Making another attempt to connect to peer {}...",
                secret_address.node_addr.node_id
            );
        }
        // We don't need to be notified, so we don't need to use the response channel.
        message_tx
            .send(EndpointMessage::Connect {
                secret_address,
                response_tx: None,
                previous_attempts: previous_attempts + 1,
            })
            .await?;
        Ok(())
    }

    async fn run(&mut self) {
        loop {
            tokio::select! {
                maybe_incoming = self.endpoint.accept() => {
                    match maybe_incoming {
                        Some(incoming) => {
                            match incoming.await {
                                Ok(conn) => {
                                    self.handle_incoming_connection(conn);
                                }
                                Err(err) => {
                                    debug!("Error while accepting peer connection: {err}");
                                }
                            }
                        }
                        None => {
                            // Endpoint was closed. Let's shut down.
                            break
                        }
                    }
                }
                maybe_message = self.message_rx.recv() => {
                    match maybe_message {
                        Some(message) => {
                            self.handle_message(message).await.expect("Failed to handle endpoint message");
                        }
                        None => {
                            // Our message channel was closed? Let's shut down.
                            break
                        }
                    }
                }
            }
        }
    }

    fn handle_incoming_connection(&self, conn: iroh::endpoint::Connection) {
        let node_id = conn
            .remote_node_id()
            .expect("Connection should have a node ID");

        info!("Peer connected: {}", &node_id);

        let my_passphrase_clone = self.my_passphrase.clone();
        let document_handle_clone = self.document_handle.clone();
        tokio::spawn(async move {
            if let Err(err) = Self::handle_peer(
                document_handle_clone,
                conn,
                PeerAuth::MyPassphrase(my_passphrase_clone),
            )
            .await
            {
                warn!("Incoming connection failed: {err}");
            }

            info!("Peer disconnected: {node_id}",);
        });
    }

    async fn handle_peer(
        document_handle: DocumentActorHandle,
        conn: iroh::endpoint::Connection,
        auth: PeerAuth,
    ) -> Result<()> {
        let connection = IrohConnection::new(conn, auth).await?;
        let syncer = SyncActor::new(document_handle, Box::new(connection));
        syncer.run().await
    }
}

// Sends/receives PeerMessages to/from and Iroh connection.
struct IrohConnection {
    send: SendStream,
    message_rx: mpsc::Receiver<Result<PeerMessage>>,
}

impl IrohConnection {
    async fn new(conn: iroh::endpoint::Connection, auth: PeerAuth) -> Result<Self> {
        let (send, receive) = match auth {
            PeerAuth::YourPassphrase(passphrase) => {
                let (mut send, recv) = conn.open_bi().await?;

                send.write_all(&passphrase.to_bytes()).await?;

                (send, recv)
            }
            PeerAuth::MyPassphrase(passphrase) => {
                let (send, mut recv) = conn.accept_bi().await?;

                let mut received_passphrase = [0; 32];
                recv.read_exact(&mut received_passphrase).await?;

                // Guard against timing attacks.
                if !constant_time_eq::constant_time_eq(&received_passphrase, &passphrase.to_bytes())
                {
                    bail!("Peer provided incorrect passphrase.");
                }

                (send, recv)
            }
        };

        let (message_tx, message_rx) = mpsc::channel(1);

        tokio::spawn(async move {
            let _ = Self::read_loop(receive, message_tx).await;
        });

        Ok(Self { send, message_rx })
    }

    async fn read_loop(
        mut receive: RecvStream,
        message_tx: mpsc::Sender<Result<PeerMessage>>,
    ) -> Result<()> {
        loop {
            let result = Self::read_next(&mut receive).await;

            message_tx.send(result).await?;
        }
    }

    async fn read_next(receive: &mut RecvStream) -> Result<PeerMessage> {
        let mut message_len_buf = [0; 4];
        receive.read_exact(&mut message_len_buf).await?;
        let byte_count = u32::from_be_bytes(message_len_buf);

        let mut bytes = vec![0; byte_count as usize];
        receive.read_exact(&mut bytes).await?;
        from_bytes(&bytes).context("Failed to convert bytes to PeerMessage")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    /// Create a temp dir with the `.teamtype/` subdirectory that `get_keypair` expects.
    fn make_temp_base_dir() -> tempfile::TempDir {
        let dir = tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join(".teamtype")).unwrap();
        dir
    }

    // ── URL validation ─────────────────────────────────────────────────────────

    #[tokio::test]
    async fn build_endpoint_invalid_relay_url_returns_error() {
        let dir = make_temp_base_dir();
        let result =
            ConnectionManager::build_endpoint(dir.path(), Some("not-a-url".to_string()), None)
                .await;
        assert!(result.is_err(), "expected Err for invalid relay URL");
    }

    #[tokio::test]
    async fn build_endpoint_invalid_discovery_url_returns_error() {
        let dir = make_temp_base_dir();
        let result =
            ConnectionManager::build_endpoint(dir.path(), None, Some("not-a-url".to_string()))
                .await;
        assert!(result.is_err(), "expected Err for invalid discovery URL");
    }

    // ── successful builds ──────────────────────────────────────────────────────

    #[tokio::test]
    async fn build_endpoint_default_succeeds_and_has_discovery() {
        let dir = make_temp_base_dir();
        let (endpoint, _passphrase) = ConnectionManager::build_endpoint(dir.path(), None, None)
            .await
            .expect("default build_endpoint should succeed");

        // n0's discovery_n0() always installs a discovery service.
        assert!(
            endpoint.discovery().is_some(),
            "default endpoint should have discovery configured"
        );
        endpoint.close().await;
    }

    #[tokio::test]
    async fn build_endpoint_custom_relay_succeeds_and_retains_discovery() {
        let dir = make_temp_base_dir();
        // Use an unreachable URL: bind() succeeds since relay connection is lazy.
        let (endpoint, _passphrase) = ConnectionManager::build_endpoint(
            dir.path(),
            Some("https://relay.example.com".to_string()),
            None,
        )
        .await
        .expect("custom-relay build_endpoint should succeed");

        // When only relay is custom, we manually re-add the three n0 discovery services.
        assert!(
            endpoint.discovery().is_some(),
            "custom-relay endpoint should still have discovery configured"
        );
        endpoint.close().await;
    }

    #[tokio::test]
    async fn build_endpoint_custom_discovery_succeeds_and_has_discovery() {
        let dir = make_temp_base_dir();
        let (endpoint, _passphrase) = ConnectionManager::build_endpoint(
            dir.path(),
            None,
            Some("https://discovery.example.com/pkarr".to_string()),
        )
        .await
        .expect("custom-discovery build_endpoint should succeed");

        assert!(
            endpoint.discovery().is_some(),
            "custom-discovery endpoint should have discovery configured"
        );
        endpoint.close().await;
    }

    #[tokio::test]
    async fn build_endpoint_both_custom_succeeds_and_has_discovery() {
        let dir = make_temp_base_dir();
        let (endpoint, _passphrase) = ConnectionManager::build_endpoint(
            dir.path(),
            Some("https://relay.example.com".to_string()),
            Some("https://discovery.example.com/pkarr".to_string()),
        )
        .await
        .expect("fully-custom build_endpoint should succeed");

        assert!(
            endpoint.discovery().is_some(),
            "fully-custom endpoint should have discovery configured"
        );
        endpoint.close().await;
    }
}

#[async_trait]
impl Connection<PeerMessage> for IrohConnection {
    async fn send(&mut self, message: PeerMessage) -> Result<()> {
        let bytes: Vec<u8> =
            to_allocvec(&message).context("Failed to convert PeerMessage to bytes")?;
        let byte_count =
            u32::try_from(bytes.len()).expect("Converting a length to u32 should work");

        self.send.write_all(&byte_count.to_be_bytes()).await?;
        self.send.write_all(&bytes).await?;

        Ok(())
    }

    async fn next(&mut self) -> Result<PeerMessage> {
        self.message_rx
            .recv()
            .await
            .context("Failed to await next peer message")?
    }
}
