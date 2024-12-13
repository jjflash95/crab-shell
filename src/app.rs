use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    fs::{self, File},
    io::{stdout, BufRead as _, BufReader, Error, ErrorKind, Read, Write},
    path::{Path, PathBuf},
};

use termion::{cursor, raw::IntoRawMode as _};

use crate::{
    exec::{exec_node, StdChannels, WaitableProcess as _},
    git,
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
    breaker: Option<bool>,
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
            breaker: None,
        };
        Sourcer::source_default_path(&mut this);
        Ok(this)
    }

    pub fn reset_branch(&mut self) {
        self.branch = git::get_current_branch();
    }

    pub fn get_var(&self, key: &str) -> Option<String> {
        self.locals
            .get(key)
            .map(ToOwned::to_owned)
            .or_else(|| std::env::var(key).ok())
    }

    pub fn enable_breaker(&mut self) {
        self.breaker = Some(false);
    }

    pub fn disable_breaker(&mut self) {
        self.breaker = None;
    }

    // is called when the break program is executed, if the breaker is `None` it returns false,
    // meaning that we are not inside a loop, otherwise sets the breaker as `Some(true)` and
    // returns true
    pub fn toggle_breaker(&mut self) -> bool {
        if self.breaker.is_none() {
            return false;
        }

        self.breaker = Some(true);
        true
    }

    pub fn should_break(&self) -> bool {
        self.breaker.is_some() && self.breaker.unwrap()
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

    pub fn source_from_text(s: &str, app: &mut AppState) {
        let tokens: Vec<_> = Tokenizer::new(s).collect();
        let program = parser::generate_program(tokens.iter().peekable());
        for node in program {
            if let Err(e) =
                exec_node(&node, StdChannels::default(), app).and_then(|pid| pid.wait_for(|_| true))
            {
                eprintln!("source: failed exec: {e}\r\n{}\r\n", node);
            }
        }
    }

    pub fn source_from_file<S: AsRef<Path>>(file: S, app: &mut AppState) -> Result<(), Error> {
        let mut f = File::open(file.as_ref())?;
        let mut contents = vec![];
        let _ = f.read_to_end(&mut contents);
        let s = String::from_utf8_lossy(&contents);
        Sourcer::source_from_text(&s, app);
        Ok(())
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

    // formats both buffers into a single string without adjusting the cursor position
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

    // returns the amount of lines the buffer occupies,
    // this is the amount of newlines in the buffer + the amount of vertical spaces a line longer
    // than the terminal width occupies
    pub fn scrollback(&self, max_width: usize, prompt_len: Option<usize>) -> usize {
        let mut lines = 0;
        let mut width = 0;
        for char in self.left.iter() {
            width += 1;
            if lines == 0 && prompt_len.unwrap_or_default() + width == max_width {
                lines += 1;
                width = 0;
            }
            if *char == '\n' || width == max_width {
                lines += 1;
                width = 0;
            }
        }
        for char in self.right.iter().rev() {
            width += 1;
            if *char == '\n' || width == max_width {
                lines += 1;
                width = 0;
            }
        }

        if (self.right.is_empty() && self.left.last().is_some_and(|c| *c == '\n'))
            || (self.right.first().is_some_and(|c| *c == '\n'))
        {
            lines -= 1
        }
        lines
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

    pub fn src(&self) -> Option<&Path> {
        self.src.as_deref()
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
        #[allow(deprecated)]
        if let Some(home) = std::env::home_dir() {
            return home
                .join(format!(".{}", crate::APP_NAME_SHORT))
                .join(HISTORY_FILE_NAME);
        }

        PathBuf::default()
    }

    pub fn save(&self) -> Result<(), Error> {
        let Some(src) = &self.src else {
            return Err(Error::new(ErrorKind::Other, "No source file"));
        };
        let mut file = fs::OpenOptions::new().append(true).open(src)?;
        for cmd in self.buffer[self.save_from..].iter() {
            file.write_all(format!("{}\n", cmd).as_bytes())?;
        }
        Ok(())
    }

    pub fn reload(&mut self) -> Result<(), Error> {
        let mut new_hist = Self::from_path(Self::get_default_path())?;
        std::mem::swap(self, &mut new_hist);
        Ok(())
    }
}

impl Debug for AppState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AppState")
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
        Sourcer::source_from_text(src, &mut app);
        assert_eq!(app.get_var("var1"), Some("1".to_string()));
        assert_eq!(app.get_var("var2"), Some("345".to_string()));
        assert_eq!(app.get_var("var3"), Some("3".to_string()));
        assert_eq!(app.get_var("var4"), Some("4".to_string()));
        assert_eq!(app.get_var("var5"), Some("5".to_string()));
    }
}
