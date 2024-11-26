use std::{
    env::home_dir,
    fs::{DirEntry, FileType, ReadDir},
    io::Error,
    path::{Path, PathBuf},
};

use nix::NixPath as _;

use crate::entry_to_path_str;

pub trait AutoCompStrategy {
    fn get_entries(&self, tip: &str) -> Vec<AutoCompEntry>;
}

pub struct DirectoryBuilder;

pub struct GitCommandBuilder;

pub enum AutoCompEntry {
    Dir(String, FileType),
    RawCmd(&'static str),
}

impl AutoCompEntry {
    pub fn as_str(&self) -> &str {
        use AutoCompEntry::*;

        match self {
            Dir(s, _) => s.as_str(),
            RawCmd(s) => s,
        }
    }
}

impl AutoCompStrategy for DirectoryBuilder {
    fn get_entries(&self, tip: &str) -> Vec<AutoCompEntry> {
        if let Some(dirs) = Self::list_or_parent(tip) {
            return dirs
                .into_iter()
                .filter_map(|d| d.ok())
                .filter_map(|d| {
                    d.file_type()
                        .map(|ft| (entry_to_path_str(&d), ft))
                        .ok()
                })
                .map(|(p, ft)| AutoCompEntry::Dir(p, ft))
                .collect();
        }
        vec![]
    }
}

impl AutoCompStrategy for GitCommandBuilder {
    fn get_entries(&self, tip: &str) -> Vec<AutoCompEntry> {
        todo!()
    }

}

pub fn get_autocomp_strat(cmd: &str) -> Box<dyn AutoCompStrategy> {
    match cmd.trim() {
        _ if cmd.starts_with("git") => Box::new(GitCommandBuilder),
        _ => Box::new(DirectoryBuilder),
    }
}

impl DirectoryBuilder {
    pub fn list_or_parent(dir: &str) -> Option<ReadDir> {
        if let Ok(dirs) = Self::list_dir(dir) {
            Some(dirs)
        } else {
            Self::list_dir(PathBuf::from(dir).parent()?).ok()
        }
    }

    pub fn list_dir<P: AsRef<Path> + std::fmt::Debug>(dir: P) -> Result<ReadDir, Error> {
        if dir.as_ref().is_empty() {
            std::fs::read_dir("./")
        } else {
            std::fs::read_dir(dir)
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
    let home = home_dir().unwrap_or_default();
    let home_str = home.as_path().to_str().unwrap_or_default();
    format!("{}/{}", home_str, s.split_at(2).1)
}
