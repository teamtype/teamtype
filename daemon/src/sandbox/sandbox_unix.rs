use std::{fs, io};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use crate::sandbox::Sandbox;

#[derive(Clone, Default, Debug)]
pub struct UnixSandbox{}
impl Sandbox for UnixSandbox {
    fn set_permissions(&self, canonical_dir_path: PathBuf, mode: u32) -> io::Result<()> {
        let permissions = fs::Permissions::from_mode(mode);
        fs::set_permissions(canonical_dir_path, permissions)
    }
}