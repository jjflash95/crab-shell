use std::{
    fs::{self, DirEntry, ReadDir},
    io::Error,
    path::{Path, PathBuf},
};

#[allow(deprecated)]
use std::env::home_dir;

use nix::NixPath;

pub struct Navigator;

impl Navigator {
    pub fn list_or_parent(dir: &str) -> Option<ReadDir> {
        if let Ok(dirs) = Self::list_dir(dir) {
            Some(dirs)
        } else {
            Self::list_dir(PathBuf::from(dir).parent()?).ok()
        }
    }

    pub fn list_dir<P: AsRef<Path> + std::fmt::Debug>(dir: P) -> Result<ReadDir, Error> {
        if dir.as_ref().is_empty() {
            fs::read_dir("./")
        } else {
            fs::read_dir(dir)
        }
    }
}

pub fn is_prefixed_with(needle: &str, entry: &DirEntry) -> bool {
    let p = entry.path();
    let Some(s) = p.to_str() else { return false };
    s.strip_prefix("./")
        .unwrap_or(s)
        .starts_with(needle.strip_prefix("./").unwrap_or(needle))
}

pub fn expand_home_symbol(s: &str) -> String {
    if !s.starts_with("~/") {
        return s.to_string();
    }

    // not intended to work on windows anyway
    #[allow(deprecated)]
    let home = home_dir().unwrap_or_default();
    let home_str = home.as_path().to_str().unwrap_or_default();
    format!("{}/{}", home_str, s.split_at(2).1)
}
