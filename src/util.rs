use memfd::{Memfd, MemfdOptions};
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};

pub struct TempFile {
    _file: Memfd,
    path: PathBuf,
}

impl TempFile {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

pub fn tempfile() -> TempFile {
    let pid = std::process::id();
    let file = MemfdOptions::new().create("").unwrap();
    let fd = file.as_raw_fd();
    let path = PathBuf::from(format!("/proc/{pid}/fd/{fd}"));
    TempFile { _file: file, path }
}
