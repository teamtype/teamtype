const SERVER: &str = "md.ha.si";

use anyhow::{bail, Result};
use async_stream::stream;
use futures::stream;
use futures_util::FutureExt;
use futures_util::StreamExt;
use reqwest::cookie::{CookieStore, Jar};
use rust_socketio::{
    asynchronous::{Client, ClientBuilder},
    Payload, TransportType,
};
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;
use tokio::io::AsyncReadExt;
use tokio::io::BufReader;
use tokio_util::codec::FramedRead;
use tokio_util::codec::LinesCodec;

async fn get_cookie(url: &str) -> Result<String> {
    // Create a cookie jar to store cookies
    let cookie_jar = Arc::new(Jar::default());

    // Create a client with the cookie jar
    let client = reqwest::Client::builder()
        .cookie_store(true)
        .cookie_provider(cookie_jar.clone())
        .build()?;

    // Send a GET request to the URL
    let response = client.get(url).send().await?;

    // Ensure the response was successful
    if !response.status().is_success() {
        bail!(format!("Failed to fetch URL: {}", response.status()));
    }

    // Retrieve cookies from the cookie jar
    if let Some(cookies) = cookie_jar.cookies(&url.parse()?) {
        // Return the cookies as a string
        Ok(cookies.to_str()?.to_string())
    } else {
        bail!("No cookies found.")
    }
}

type EditorMessage = u8;

#[tokio::main]
async fn main() {
    //// Set up stream from be stdin, streamed byte by byte.
    //let mut stdin = tokio::io::stdin();
    //let editor_stream = stream! {
    //    let mut buf = [0; 1];
    //    loop {
    //        match stdin.read_exact(&mut buf).await {
    //            Ok(_) => {
    //                yield buf[0];
    //            }
    //            Err(e) => {
    //                eprintln!("Error reading from stdin: {:?}", e);
    //                break;
    //            }
    //        }
    //    }
    //};

    //let mut socket_read = FramedRead::new(socket_read, LinesCodec::new());

    //let mut editor_stream = Box::pin(editor_stream);

    let mut editor_stream = FramedRead::new(BufReader::new(tokio::io::stdin()), LinesCodec::new());

    let mut running = true;
    while running {
        tokio::select! {
            editor_message_maybe = editor_stream.next() => {
                match editor_message_maybe {
                    Some(editor_message) => {
                        // integrate editor message
                        dbg!(editor_message);
                    }
                    None => {
                        // editor stream ended
                        running = false;
                    }
                }
            }
        }
    }
    println!("done");

    /*
    // define a callback which is called when a payload is received
    // this callback gets the payload as well as an instance of the
    // socket to communicate with the server
    let callback = |payload: Payload, socket: Client| {
        async move {
            match payload {
                Payload::Text(str) => println!("Received: {:?}", str),
                Payload::Binary(bin_data) => println!("Received bytes: {:#?}", bin_data),
                Payload::String(_) => todo!(),
            }
            //socket
            //    .emit("test", json!({"got ack": true}))
            //    .await
            //    .expect("Server unreachable");
        }
        .boxed()
    };

    let selection_callback = |payload: Payload, socket: Client| {
        async move {
            match payload {
                Payload::Text(str) => println!("Selection: {:?}", str),
                Payload::Binary(bin_data) => todo!(),
                Payload::String(_) => todo!(),
            }
        }
        .boxed()
    };

    let operation_callback = |payload: Payload, socket: Client| {
        async move {
            match payload {
                Payload::Text(str) => println!("Operation: {:?}", str),
                Payload::Binary(bin_data) => todo!(),
                Payload::String(_) => todo!(),
            }
            socket
                .emit(
                    "operation",
                    vec![
                        json!(13),
                        json!(["c", 2]), // doesn't need to have the proper "length"!
                        json!({"ranges": [{"anchor": 0, "head": 0}]}),
                    ],
                )
                .await
                .expect("Server unreachable");
        }
        .boxed()
    };

    let server = "https://md.ha.si";
    //let server = "http://localhost:3000";
    //let cookie = get_cookie(&format!("{server}/socket.io/?transport=polling"))
    let cookie = get_cookie(&format!("{server}"))
        .await
        .expect("Failed to get cookie");
    dbg!(&cookie);

    // get a socket that is connected to the admin namespace
    let socket = ClientBuilder::new(format!("{server}/socket.io/?noteId=test"))
        //.namespace("/blubb")
        .transport_type(TransportType::Polling)
        .opening_header("Cookie", cookie)
        //.auth(json!({"noteId": "test"}))
        .on("connect", |_, _| {
            async move { println!("connected") }.boxed()
        })
        .on("selection", selection_callback)
        .on("doc", callback)
        .on("operation", operation_callback)
        .on("error", |err, _| {
            async move { eprintln!("Error: {:#?}", err) }.boxed()
        })
        .connect()
        .await
        .expect("Connection failed");

    //// define a callback, that's executed when the ack got acked
    //let ack_callback = |message: Payload, _: Client| {
    //    async move {
    //        println!("Yehaa! My ack got acked?");
    //        println!("Ack data: {:#?}", message);
    //    }
    //    .boxed()
    //};

    //let json_payload = json!({"myAckData": 123});
    //// emit with an ack
    //socket
    //    .emit_with_ack("test", json_payload, Duration::from_secs(2), ack_callback)
    //    .await
    //    .expect("Server unreachable");

    println!("myyyy connected");

    //let json_payload = json!({});
    //socket
    //    .emit("version", json_payload)
    //    .await
    //    .expect("Server unreachable");
    //println!("done");

    socket
        .emit("selection", json!({"ranges": [{"anchor": 0, "head": 2}]}))
        .await
        .expect("Server unreachable");

    //socket.disconnect().await.expect("Disconnect failed");

    loop {
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
    */
}
