
use std::{fs, os::unix::fs::PermissionsExt, path::Path};
use anyhow::{Context, bail};
use futures::StreamExt;
use tokio::{
    net::{UnixListener, UnixStream},
};
use tokio_util::codec::{FramedRead, FramedWrite};
use tracing::{debug, error, info};
use crate::cli_ask::ask;
use crate::daemon::{DocMessage, DocumentActorHandle};
use crate::editor::{Editor, EditorId, IncomingProtocolCodec, OutgoingProtocolCodec};
use crate::editor_protocol::{EditorProtocolMessageError, JSONRPCResponse, OutgoingMessage};
use crate::sandbox::Sandbox;

#[derive(Clone, Debug)]
pub struct EditorUnix{
 pub sandbox: Box<dyn Sandbox>
}

impl Editor for EditorUnix {
    /// # Panics
    ///
    /// Will panic if we fail to listen on the socket, or if we fail to accept an incoming connection.
    fn spawn_socket_listener(
        &self,
        socket_path: &Path,
        document_handle: DocumentActorHandle,
    ) -> anyhow::Result<()> {
        // Make sure the parent directory of the socket is only accessible by the current user.
        if let Err(description) = is_user_readable_only(socket_path) {
            panic!("{}", description);
        }

        // Using the sandbox method here is technically unnecessary,
        // but we want to really run all path operations through the sandbox module.
        // TODO: Use correct directory as guard.
        if self.sandbox.exists(Path::new("/"), Path::new(&socket_path))
            .expect("Failed to check existence of path")
        {
            let socket_path_display = socket_path.display();
            let remove_socket = ask(&format!(
                "Detected an existing socket '{socket_path_display}'. There might be a daemon running already for this directory, or the previous one crashed. Do you want to continue?"
            ));
            if remove_socket? {
                self.sandbox.remove_file(Path::new("/"), socket_path).expect("Could not remove socket");
            } else {
                bail!("Not continuing, make sure to stop all other daemons on this directory");
            }
        }

        let listener = UnixListener::bind(socket_path)?;
        debug!("Listening on UNIX socket: {}", socket_path.display());

        tokio::spawn(async move {
            loop {
                match listener.accept().await {
                    Ok((stream, _addr)) => {
                        let id = document_handle.clone().next_editor_id();
                        let document_handle_clone = document_handle.clone();
                        tokio::spawn(async move {
                            handle_editor_connection(stream, document_handle_clone.clone(), id).await;
                        })
                    }
                    Err(err) => {
                        panic!("Error while accepting socket connection: {err}");
                    }
                };
            }
        });

        Ok(())
    }
}

async fn handle_editor_connection(
    stream: UnixStream,
    document_handle: DocumentActorHandle,
    editor_id: EditorId,
) {
    let (stream_read, stream_write) = tokio::io::split(stream);
    let mut reader = FramedRead::new(stream_read, IncomingProtocolCodec);
    let writer = FramedWrite::new(stream_write, OutgoingProtocolCodec);

    document_handle
        .send_message(DocMessage::NewEditorConnection(editor_id, writer))
        .await;
    info!("Editor #{editor_id} connected.");

    while let Some(message) = reader.next().await {
        match message {
            Ok(message) => {
                document_handle
                    .send_message(DocMessage::FromEditor(editor_id, message))
                    .await;
            }
            Err(e) => {
                let response = JSONRPCResponse::RequestError {
                    id: None,
                    error: EditorProtocolMessageError {
                        code: -32700,
                        message: format!("Invalid request: {e}"),
                        data: None,
                    },
                };
                error!("Error for JSON-RPC request: {:?}", response);
                let message = OutgoingMessage::Response(response);
                document_handle
                    .send_message(DocMessage::ToEditor(editor_id, message))
                    .await;
            }
        }
    }
    // Err(e) => {
    // }

    document_handle
        .send_message(DocMessage::CloseEditorConnection(editor_id))
        .await;
    info!("Editor #{editor_id} disconnected.");
}



fn is_user_readable_only(socket_path: &Path) -> anyhow::Result<()> {
    let parent_dir = socket_path
        .parent()
        .context("The socket path should not be the root directory")?;
    let current_permissions = fs::metadata(parent_dir)
        .with_context(|| {
            format!(
                "Expected to have access to metadata of the socket's parent directory: {}",
                parent_dir.display()
            )
        })?
        .permissions()
        .mode();
    // Group and others should not have any permissions.
    let allowed_permissions = 0o77700u32;
    if current_permissions | allowed_permissions != allowed_permissions {
        bail!(
            "For security reasons, the parent directory of the socket must only be accessible by the current user. Please run `chmod go-rwx {}`",
            parent_dir.display()
        );
    }
    Ok(())
}