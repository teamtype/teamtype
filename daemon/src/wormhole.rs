// SPDX-FileCopyrightText: 2025 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2025 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Result;
use magic_wormhole::{transfer, AppConfig, AppID, Code, MailboxConnection, Wormhole};
use std::{borrow::Cow, str::FromStr, time::Duration};
use tokio::time::sleep;
use tracing::{error, info, warn};

pub async fn put_secret_address_into_wormhole(address: &str, rendezvous_url: Option<String>) {
    let payload: Vec<u8> = address.into();
    let config = build_magic_wormhole_config(rendezvous_url);

    tokio::spawn(async move {
        loop {
            let Ok(mailbox_connection) = MailboxConnection::create(config.clone(), 2).await else {
                error!(
                    "Failed to share join code via magic wormhole. Restart Teamtype to try again."
                );
                return;
            };
            let code = mailbox_connection.code().clone();

            info!(
                "\n\tOne other person can use this to connect to you:\n\n\tteamtype join {}\n",
                &code
            );

            if let Ok(mut wormhole) = Wormhole::connect(mailbox_connection).await {
                let _ = wormhole.send(payload.clone()).await;
            } else {
                warn!("Failed to share secret address. Did your peer mistype the join code?");
            }

            // Print a new join code in the next iteration of the foor loop, to allow more people
            // to join.
            sleep(Duration::from_millis(500)).await;
        }
    });
}

pub async fn get_secret_address_from_wormhole(
    code: &str,
    rendezvous_url: Option<String>,
) -> Result<String> {
    let config = build_magic_wormhole_config(rendezvous_url);

    let mut wormhole =
        Wormhole::connect(MailboxConnection::connect(config, Code::from_str(code)?, false).await?)
            .await?;
    let bytes = wormhole.receive().await?;
    Ok(String::from_utf8(bytes)?)
}

fn build_magic_wormhole_config(rendezvous_url: Option<String>) -> AppConfig<transfer::AppVersion> {
    let mut config = transfer::APP_CONFIG.id(AppID::new("teamtype"));

    if let Some(url) = rendezvous_url {
        info!("Using rendezvous url {}", url);
        config = config.rendezvous_url(Cow::Owned(url));
    }
    config
}
