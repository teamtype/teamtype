use anyhow::Result;
use teamtype::{Config, Interactions, UserInterface};
use tracing::{debug, info, warn};
use tracing_subscriber::prelude::*;
use tracing_web::MakeWebConsoleWriter;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::js_sys;

pub struct TestInteractions {
    log: js_sys::Function,
    inform: js_sys::Function,
    warn: js_sys::Function,
}

impl Interactions for TestInteractions {
    fn confirm(&self, question: &str) -> Result<bool> {
        debug!("Answering '{question}' with 'no'");
        Ok(false)
    }

    fn log(&self, message: &str) {
        self.log
            .call1(&JsValue::NULL, &JsValue::from_str(message))
            .expect("Failed to call log");
    }

    fn inform(&self, message: &str) {
        self.inform
            .call1(&JsValue::NULL, &JsValue::from_str(message))
            .expect("Failed to call inform");
    }

    fn warn(&self, message: &str) {
        self.warn
            .call1(&JsValue::NULL, &JsValue::from_str(message))
            .expect("Failed to call warn");
    }
}

#[wasm_bindgen]
pub async fn launch(
    log: js_sys::Function,
    inform: js_sys::Function,
    warn: js_sys::Function,
) -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

    let console_writer = MakeWebConsoleWriter::new();
    let fmt_layer = tracing_subscriber::fmt::layer()
        .with_ansi(false)
        .without_time()
        .with_writer(console_writer);

    tracing_subscriber::registry().with(fmt_layer).init();

    let config = Config {
        emit_join_code: true,

        // Default mailbox server doesn't seem to support wss://.
        magic_wormhole_relay: Some("wss://mailbox.mw.leastauthority.com/v1".to_string()),
        ..Config::default()
    };

    let ui = &UserInterface::new(TestInteractions { log, inform, warn });

    let _daemon = teamtype::run_daemon(config, false, ui).await;

    Ok(())
}
