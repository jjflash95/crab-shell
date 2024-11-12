use std::{
    collections::HashMap,
    fmt::Display,
    fs,
    io::{stdout, BufRead as _, BufReader, Error, Write as _},
    path::{Path, PathBuf},
};

use termion::{cursor, raw::IntoRawMode as _};

use crate::{exec_tree, git, lexer::Tokenizer, parser, RawTerm, APP_NAME_SHORT};

const HISTORY_FILE_NAME: &str = ".history";

pub struct AppState {
    pub term: RawTerm,
    pub buf: CharBuffer,
    pub history: History,
    pub branch: Option<String>,
    pub locals: HashMap<String, String>,
}

#[derive(Default)]
pub struct CharBuffer {
    pub left: Vec<char>,
    pub right: Vec<char>,
}

#[derive(Default)]
pub struct History {
    src: Option<PathBuf>,
    save_from: usize,
    index: Option<usize>,
    buffer: Vec<String>,
    current: Option<String>,
}

pub struct Sourcer;

impl AppState {
    pub fn new() -> Result<Self, Error> {
        let mut this = Self {
            term: stdout().into_raw_mode()?,
            buf: CharBuffer::new(),
            history: History::new(),
            branch: git::get_current_branch(),
            locals: HashMap::new(),
        };

        Sourcer::source(&mut this);
        Ok(this)
    }

    pub fn reset_branch(&mut self) {
        self.branch = git::get_current_branch();
    }

    pub fn get_var(&self, key: &str) -> String {
        self.locals
            .get(key)
            .map(ToOwned::to_owned)
            .or_else(|| std::env::var(key).ok())
            .unwrap_or_default()
    }
}

impl Display for AppState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.buf)
    }
}

impl Sourcer {
    pub fn source(app: &mut AppState) {
        Sourcer::source_from_file(Self::get_default_path(), app);
    }

    pub fn source_from_file<S: AsRef<Path>>(file: S, app: &mut AppState) {
        if let Ok(history) = History::from_path(file.as_ref()) {
            for (line, cmd) in history.buffer.iter().enumerate() {
                if cmd.trim().is_empty() {
                    continue;
                }

                let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
                let Ok((_, ast)) = parser::ast(tokens.iter().peekable()) else {
                    eprint!("source: line {}: failed processing {cmd}\r\n", line + 1);
                    continue;
                };
                match exec_tree(&ast, app) {
                    Ok(_) => {}
                    Err(e) => eprint!("source: line {}:, failed exec {e}\r\n", line + 1),
                }
            }
        }
    }

    fn get_default_path() -> PathBuf {
        match std::env::home_dir() {
            Some(hd) => hd.join(format!(".{}rc", APP_NAME_SHORT)),
            None => PathBuf::new(),
        }
    }
}

impl CharBuffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, c: char) {
        self.left.push(c)
    }

    // pop a character from left and push into right buffer, returning the moved character
    // happens when user presses -> key (right)
    pub fn move_l2r(&mut self) -> Option<char> {
        if let Some(char) = self.left.pop() {
            self.right.push(char);
            return Some(char);
        }
        None
    }

    // pop a character from right and push into left buffer, returning the moved character
    // happens when user presses <- key (left)
    pub fn move_r2l(&mut self) -> Option<char> {
        if let Some(char) = self.right.pop() {
            self.left.push(char);
            return Some(char);
        }
        None
    }

    pub fn string_nc(&self) -> String {
        format!(
            "{}{}",
            self.left.iter().collect::<String>(),
            self.right.iter().rev().collect::<String>()
        )
    }

    pub fn is_empty(&self) -> bool {
        self.left.is_empty() && self.right.is_empty()
    }
}

impl Display for CharBuffer {
    // Displays the buffer as a string, rendering the cursor in the current position
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = write!(f, "{}", self.string_nc());
        if !self.right.is_empty() {
            // HACK: for some reason cursor::Left(0) moves one space to the left
            // regardless, hence this condition
            write!(f, "{}", cursor::Left(self.right.len() as u16))?;
        }
        res
    }
}

impl History {
    pub fn new() -> Self {
        Self::from_path(Self::get_default_path()).unwrap_or_default()
    }

    pub fn from_path<P: AsRef<Path> + Into<PathBuf>>(p: P) -> Result<Self, Error> {
        let path = p.as_ref();
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent)?;
            }
        }
        let handle = if path.exists() {
            fs::File::open(path)?
        } else {
            let _ = fs::File::create(path)?;
            fs::File::open(path)?
        };
        let mut buffer = vec![];
        for line in BufReader::new(handle).lines().map_while(Result::ok) {
            buffer.push(line.trim().to_string())
        }

        Ok(Self {
            src: Some(p.into()),
            save_from: buffer.len(),
            index: None,
            current: None,
            buffer,
        })
    }

    pub fn get_prev(&mut self) -> Option<&str> {
        if self.buffer.is_empty() {
            return None;
        }

        let index = self.index.unwrap_or(self.buffer.len()).saturating_sub(1);
        self.index = Some(index);
        Some(&self.buffer[index])
    }

    pub fn get_next(&mut self) -> Option<&str> {
        if self.buffer.is_empty() || self.index.is_none() {
            return None;
        }
        let index = self.index.unwrap() + 1;
        if index >= self.buffer.len() {
            self.index = None;
            return self.current.as_deref();
        };

        self.index = Some(index);
        Some(&self.buffer[index])
    }

    pub fn push(&mut self, entry: String) {
        self.index.take();
        self.buffer.push(entry)
    }

    pub fn set_current(&mut self, current: String) {
        self.current = Some(current)
    }

    pub fn all(&self) -> &[String] {
        &self.buffer
    }

    pub fn get_default_path() -> PathBuf {
        if let Some(home) = std::env::home_dir() {
            return home
                .join(format!(".{}", crate::APP_NAME_SHORT))
                .join(HISTORY_FILE_NAME);
        }

        PathBuf::default()
    }
}

impl Drop for History {
    fn drop(&mut self) {
        let Some(src) = &self.src else { return };
        let Ok(mut file) = fs::OpenOptions::new().append(true).open(src) else {
            return;
        };
        for cmd in self.buffer[self.save_from..].iter() {
            let _ = file.write_all(format!("{}\n", cmd).as_bytes());
        }
    }
}
