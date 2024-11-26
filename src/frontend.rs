use crate::{app::AppState, MatchedString};
use itertools::Itertools;
use std::{
    cmp::{self, Ordering},
    fmt::Display,
    fs::{DirEntry, FileType},
    io::{Error, Stdout, Write},
    os::unix::{ffi::OsStrExt, fs::FileTypeExt}, path::PathBuf,
};
use termion::{
    clear,
    color::{Bg, Fg, Rgb},
    cursor,
    raw::RawTerminal,
    style, terminal_size,
};

pub const SALMON: Rgb = Rgb(232, 175, 151);
pub const FAINT_SALMON: Rgb = Rgb(208, 155, 131);
pub const LIGHT_BLACK: Rgb = Rgb(40, 40, 40);
pub const LIGHT_MAGENTA: Rgb = Rgb(215, 130, 126);
pub const GOLD: Rgb = Rgb(246, 193, 119);
pub const MUSK_GREEN: Rgb = Rgb(110, 170, 140);
pub const LIGHT_BLUE: Rgb = Rgb(156, 207, 216);
pub const INTENSE_BLACK: Rgb = Rgb(20, 20, 20);

pub type RawTerm = RawTerminal<Stdout>;

#[derive(Debug)]
struct PromptBar;

#[derive(Debug)]
struct FuzzyFindEntry;

#[derive(Debug)]
struct FileSearchResult;

#[derive(Debug)]
struct DirSearchResult;

#[derive(Debug)]
struct SymlinkSearchResult;

#[derive(Debug)]
struct SocketSearchResult;

#[derive(Debug)]
enum SearchResult {
    File,
    Dir,
    Symlink,
    Socket,
}

trait RgbFromEnv {
    fn key_fg() -> &'static str;
    fn key_bg() -> &'static str;

    fn default_fg() -> Rgb;
    fn default_bg() -> Rgb;

    fn env_bg() -> String {
        std::env::var(Self::key_bg()).unwrap_or_default()
    }

    fn env_fg() -> String {
        std::env::var(Self::key_fg()).unwrap_or_default()
    }

    fn parse_from_str(s: &str) -> Option<Rgb> {
        let mut parts = s.split("-");
        let r = parts.next()?.parse().ok()?;
        let g = parts.next()?.parse().ok()?;
        let b = parts.next()?.parse().ok()?;
        Some(Rgb(r, g, b))
    }

    fn color_fg() -> Fg<Rgb> {
        Fg(Self::parse_from_str(&Self::env_fg()).unwrap_or(Self::default_fg()))
    }

    fn color_bg() -> Bg<Rgb> {
        Bg(Self::parse_from_str(&Self::env_bg()).unwrap_or(Self::default_bg()))
    }
}

impl Display for PromptBar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", Self::color_fg(), Self::color_bg())
    }
}

impl RgbFromEnv for PromptBar {
    fn key_fg() -> &'static str {
        "PROMPT_BAR_FG"
    }

    fn key_bg() -> &'static str {
        "PROMPT_BAR_BG"
    }

    fn default_fg() -> Rgb {
        SALMON
    }

    fn default_bg() -> Rgb {
        LIGHT_BLACK
    }
}

impl RgbFromEnv for FuzzyFindEntry {
    fn key_bg() -> &'static str {
        "FUZZY_FIND_ENTRY_BG"
    }

    fn key_fg() -> &'static str {
        "FUZZY_FIND_ENTRY_FG"
    }

    fn default_bg() -> Rgb {
        LIGHT_BLACK
    }

    fn default_fg() -> Rgb {
        SALMON
    }
}

impl RgbFromEnv for FileSearchResult {
    fn key_fg() -> &'static str {
        "FILE_SEARCH_RESULT_FG"
    }

    fn key_bg() -> &'static str {
        "FILE_SEARCH_RESULT_BG"
    }

    fn default_fg() -> Rgb {
        LIGHT_BLUE
    }

    fn default_bg() -> Rgb {
        INTENSE_BLACK
    }
}

impl RgbFromEnv for DirSearchResult {
    fn key_fg() -> &'static str {
        "DIR_SEARCH_RESULT_FG"
    }

    fn key_bg() -> &'static str {
        "DIR_SEARCH_RESULT_BG"
    }

    fn default_fg() -> Rgb {
        FAINT_SALMON
    }

    fn default_bg() -> Rgb {
        INTENSE_BLACK
    }
}

impl RgbFromEnv for SymlinkSearchResult {
    fn key_fg() -> &'static str {
        "SYMLINK_SEARCH_RESULT_FG"
    }

    fn key_bg() -> &'static str {
        "SYMLINK_SEARCH_RESULT_BG"
    }

    fn default_fg() -> Rgb {
        GOLD
    }

    fn default_bg() -> Rgb {
        INTENSE_BLACK
    }
}

impl RgbFromEnv for SocketSearchResult {
    fn key_fg() -> &'static str {
        "SOCKET_SEARCH_RESULT_FG"
    }

    fn key_bg() -> &'static str {
        "SOCKET_SEARCH_RESULT_BG"
    }

    fn default_fg() -> Rgb {
        MUSK_GREEN
    }

    fn default_bg() -> Rgb {
        INTENSE_BLACK
    }
}

impl SearchResult {
    fn color_fg(&self) -> Fg<Rgb> {
        match self {
            Self::Dir => DirSearchResult::color_fg(),
            Self::File => FileSearchResult::color_fg(),
            Self::Symlink => SymlinkSearchResult::color_fg(),
            Self::Socket => SocketSearchResult::color_fg(),
        }
    }

    #[allow(unused)]
    fn color_bg(&self) -> Bg<Rgb> {
        match self {
            Self::Dir => DirSearchResult::color_bg(),
            Self::File => FileSearchResult::color_bg(),
            Self::Symlink => SymlinkSearchResult::color_bg(),
            Self::Socket => SocketSearchResult::color_bg(),
        }
    }
}

impl<'a> Display for MatchedString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let MatchedString(cmd, selected, (_, indices)) = &self;
        for (i, c) in cmd.chars().enumerate() {
            if *selected {
                // write!(f, "{}", Bg(Rgb(50, 50, 50)))?;
                write!(f, "{}", FuzzyFindEntry::color_bg())?;
            }
            if indices.contains(&i) {
                write!(f, "{}{c}{}", FuzzyFindEntry::color_fg(), style::Reset)?;
            } else {
                write!(f, "{}", c)?;
            }
        }
        if *selected {
            write!(f, "{}\x1B[K{}", FuzzyFindEntry::color_bg(), style::Reset)?;
        }
        Ok(())
    }
}
pub fn write_and_flush<T: Write, S: Display>(term: &mut T, s: S) -> Result<(), Error> {
    write!(term, "{}", s)?;
    term.flush()?;
    Ok(())
}

pub fn prompt(app: &mut AppState) -> Result<(), Error> {
    let (w, _) = terminal_size()?;
    let mut current_dir = std::env::current_dir()?
        .into_os_string()
        .into_string()
        .unwrap_or_default();
    let home = std::env::var("HOME").ok().unwrap_or_default();
    let (prefix, current_dir) = if current_dir.starts_with(&home) {
        ("~", current_dir.split_off(home.len()))
    } else {
        ("", current_dir)
    };

    let (parent, tip) = current_dir
        .rsplit_once('/')
        .map(|(p, t)| (format!("{p}/"), t))
        .unwrap_or((String::new(), &current_dir));

    write!(
        app.term,
        "\r{}\r{} {}{}{}{}{}{} {} {}",
        clear::CurrentLine,
        PromptBar,
        prefix,
        style::Faint,
        parent,
        style::NoFaint,
        style::Bold,
        tip,
        style::Reset,
        cursor::Save,
    )?;

    if let Some(branch) = &app.branch {
        write!(
            app.term,
            "{}{}{}{}",
            cursor::Right(w),
            cursor::Left(branch.len() as u16),
            branch,
            cursor::Restore
        )?;
    }

    write!(app.term, "{}", app.buf)?;
    app.term.flush()?;
    Ok(())
}

pub fn display_cmd_history<S: Display>(term: &mut RawTerm, cmds: &[S]) -> Result<(), Error> {
    let (w, h) = terminal_size()?;
    write!(term, "{}", clear::All)?;
    let cmds = if cmds.len() >= h as usize {
        &cmds[cmds.len() - (h as usize - 1)..]
    } else {
        cmds
    };

    for (i, cmd) in cmds.iter().rev().enumerate() {
        write!(
            term,
            "{}{}{}",
            style::Reset,
            cursor::Goto(3, h - i as u16 - 1),
            cmd
        )?;
    }
    write!(
        term,
        "{}> {}{}{}quick search (Esc to exit){}",
        cursor::Goto(1, h),
        cursor::Save,
        cursor::Right(w),
        cursor::Left(25_u16),
        cursor::Restore
    )?;
    term.flush()?;
    Ok(())
}

pub fn entry_to_path_str(e: &DirEntry) -> String {
    String::from_utf8_lossy(e.path().as_os_str().as_bytes()).to_string()
}

fn get_styled_search_result(ft: Option<FileType>) -> SearchResult {
    match ft {
        Some(ft) if ft.is_file() => SearchResult::File,
        Some(ft) if ft.is_dir() => SearchResult::Dir,
        Some(ft) if ft.is_symlink() => SearchResult::Symlink,
        Some(ft) if ft.is_socket() => SearchResult::Socket,
        _ => SearchResult::Socket,
    }
}

pub fn get_formatted_dirs(
    dirs: &[DirEntry],
    term_width: u16,
    padding: Option<usize>,
) -> Vec<String> {
    let max_name_length = dirs
        .iter()
        .map(|e| entry_to_path_str(e).len())
        .max()
        .unwrap_or(0);

    let dirs = dirs
        .iter()
        .sorted_by(|a, b| comp_dir_entries(a, b))
        .collect::<Vec<_>>();

    let term_width = if term_width > 50 {
        (term_width as f32 * 0.95).floor() as usize
    } else {
        term_width as usize
    };
    let column_width = max_name_length + padding.unwrap_or(4);
    let columns = cmp::max(1, term_width / column_width);
    let mut formatted = vec![];
    let mut current = String::new();

    for (i, dir) in dirs.iter().enumerate() {
        let style = get_styled_search_result(dir.file_type().ok());
        current += &format!(
            "{}{:<column_width$}{}",
            style.color_fg(),
            entry_to_path_str(dir),
            termion::style::Reset
        );
        if (i + 1) % columns == 0 {
            formatted.push(current);
            current = String::new();
        }
    }
    formatted.push(current);
    formatted
}

fn comp_dir_entries(a: &DirEntry, b: &DirEntry) -> Ordering {
    let Ok(aft) = a.file_type() else {
        return Ordering::Less;
    };
    let Ok(bft) = b.file_type() else {
        return Ordering::Greater;
    };
    match (aft, bft) {
        _ if aft.is_file() && bft.is_file() => Ordering::Equal,
        _ if aft.is_dir() && bft.is_dir() => Ordering::Equal,
        _ if aft.is_socket() && bft.is_socket() => Ordering::Equal,
        _ if aft.is_symlink() && bft.is_symlink() => Ordering::Equal,
        _ if aft.is_file() => Ordering::Greater,
        _ if aft.is_dir() && !bft.is_file() => Ordering::Greater,
        _ if aft.is_symlink() && !bft.is_file() && !bft.is_dir() => Ordering::Greater,
        _ if aft.is_socket() && !bft.is_symlink() && !bft.is_file() && !bft.is_dir() => {
            Ordering::Greater
        }
        _ => Ordering::Less,
    }
}

pub fn get_dir_common_substring(dirs: &[DirEntry]) -> Option<String> {
    let stringified: Vec<String> = dirs.iter().map(entry_to_path_str).collect();
    if stringified.is_empty() {
        return None;
    };

    let first = stringified[0].as_str();
    let common = stringified[1..]
        .iter()
        .fold(first, |acc, s| common_prefix(acc, s));

    Some(common.to_string())
}


pub fn get_common_substring(entries: &[String]) -> Option<String> {
    let first = entries[0].as_ref();
    let common = entries[1..]
        .iter()
        .fold(first, |acc, s| common_prefix(acc, s));

    Some(common.to_string())
}

fn common_prefix<'a>(a: &'a str, b: &'a str) -> &'a str {
    let min_len = a.len().min(b.len());
    let mut end = 0;

    for i in 0..min_len {
        if a.as_bytes()[i] == b.as_bytes()[i] {
            end = i + 1;
        } else {
            break;
        }
    }

    &a[0..end]
}

pub mod gradient {
    use super::*;

    pub fn print_gradient(
        line: &str,
        Rgb(sr, sg, sb): Rgb,
        Rgb(er, eg, eb): Rgb,
        stdout: &mut std::io::Stdout,
    ) -> Result<(), Error> {
        let len = line.chars().count();
        for (i, c) in line.chars().enumerate() {
            let color = interpolate((sr, sg, sb), (er, eg, eb), i as f32 / len as f32);
            write!(
                stdout,
                "{}{}{}",
                style::Bold,
                Fg(Rgb(color.0, color.1, color.2)),
                c
            )?;
        }
        write!(stdout, "\r\n")?;
        Ok(())
    }

    fn interpolate(start: (u8, u8, u8), end: (u8, u8, u8), t: f32) -> (u8, u8, u8) {
        (
            lerp(start.0, end.0, t),
            lerp(start.1, end.1, t),
            lerp(start.2, end.2, t),
        )
    }

    fn lerp(start: u8, end: u8, t: f32) -> u8 {
        (start as f32 + (end as f32 - start as f32) * t) as u8
    }
}
