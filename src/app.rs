use std::{
    collections::HashMap,
    fmt::Display,
    fs::{self, File},
    io::{stdout, BufRead as _, BufReader, Error, Read, Write as _},
    path::{Path, PathBuf},
};

use termion::{cursor, raw::IntoRawMode as _};

use crate::{
    exec::{exec_node, StdChannels, WaitableProcess as _},
    exec_tree, git,
    lexer::Tokenizer,
    parser, RawTerm, APP_NAME_SHORT,
};

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

#[derive(Debug)]
pub enum SourceError {
    FileError(String),
    Lex(String),
    Parse(String),
    Exec(String),
}

impl AppState {
    pub fn new() -> Result<Self, Error> {
        let mut this = Self {
            term: stdout().into_raw_mode()?,
            buf: CharBuffer::new(),
            history: History::new(),
            branch: git::get_current_branch(),
            locals: HashMap::new(),
        };
        Sourcer::source_default_path(&mut this);
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
    pub fn source_default_path(app: &mut AppState) {
        if let Err(e) = Sourcer::source_from_file(Self::get_default_path(), app) {
            eprintln!("source err: {:?}", e);
        }
    }

    pub fn source_from_text(s: &str, app: &mut AppState) -> Result<(), SourceError> {
        let tokens: Vec<_> = Tokenizer::new(s).collect();
        let program = parser::generate_program(tokens.iter().peekable());
        for node in program {
            if let Err(e) =
                exec_node(&node, StdChannels::default(), app).and_then(|pid| pid.wait_for(|_| true))
            {
                eprintln!("source: failed exec: {e}\r\n{}\r\n", node);
            }
        }
        Ok(())
    }

    pub fn source_from_file<S: AsRef<Path>>(
        file: S,
        app: &mut AppState,
    ) -> Result<(), SourceError> {
        let Ok(mut f) = File::open(file.as_ref()) else {
            return Err(SourceError::FileError(format!(
                "Failed to open {}",
                file.as_ref().to_str().unwrap_or("")
            )));
        };
        let mut contents = vec![];
        let _ = f.read_to_end(&mut contents);
        let s = String::from_utf8_lossy(&contents);
        Sourcer::source_from_text(&s, app)
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

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_rc() {
        let src = "
                if [ true ]; then 
                    export var1=1
                else
                    export var1=2
                        fi;


                if false; then
                    export var2=123
                else
                    export var2=345

                fi;

                for i in 3 4 5; do
                    export var$i=$i
                done
            ";

        let mut app = AppState::new().unwrap();
        Sourcer::source_from_text(src, &mut app).unwrap();
        assert_eq!(app.get_var("var1"), "1".to_string());
        assert_eq!(app.get_var("var2"), "345".to_string());
        assert_eq!(app.get_var("var3"), "3".to_string());
        assert_eq!(app.get_var("var4"), "4".to_string());
        assert_eq!(app.get_var("var5"), "5".to_string());
    }
}
