use memfd::{Memfd, MemfdOptions};
use std::os::fd::AsRawFd;
use std::path::PathBuf;

pub fn tempfile() -> (Memfd, PathBuf) {
    let pid = std::process::id();
    let file = MemfdOptions::new().create("").unwrap();
    let fd = file.as_raw_fd();
    let path = PathBuf::from(format!("/proc/{pid}/fd/{fd}"));
    (file, path)
}
