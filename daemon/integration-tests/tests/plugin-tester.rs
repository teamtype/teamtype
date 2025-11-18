use teamtype::sandbox;
use teamtype_integration_tests::socket::MockSocket;

#[tokio::main]
async fn main() {
    let dir = temp_dir::TempDir::new().expect("Failed to create temp directory");
    let mut teamtype_dir = dir.path().to_path_buf();
    println!("{:?}", &teamtype_dir);
    teamtype_dir.push(".teamtype");
    sandbox::create_dir(dir.path(), &teamtype_dir).expect("Failed to create .teamtype directory");

    let file = dir.child("file");
    sandbox::write_file(dir.path(), &file, b"").expect("Failed to create file in temp directory");

    let mut socket_path = dir.child(".teamtype");
    socket_path.push("socket");
    let mut socket = MockSocket::new(&socket_path);

    let m = socket.recv().await;
    println!("{:?}", m);
}
