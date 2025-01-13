mod app;
mod autocomp;
mod exec;
mod expand;
mod frontend;
mod git;
mod lexer;
mod nav;
mod parser;
mod utils;

use app::{AppState, CharBuffer};
use frontend::*;
use lexer::Tokenizer;
use nav::*;
use utils::*;

use exec::{exec_program, WaitableProcess};
use std::{
    io::{stdin, Error, Stdin, Write as _},
    thread,
    time::Duration,
};
use termion::{
    clear,
    cursor::{self, DetectCursorPos},
    event::Key,
    input::{Keys, TermRead},
    screen::{ToAlternateScreen, ToMainScreen},
    terminal_size,
};

#[allow(dead_code)]
const APP_NAME: &str = "crab";
const APP_NAME_SHORT: &str = "csh";

fn main() -> Result<(), Error> {
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");
    let mut app = AppState::new()?;
    let mut input = stdin().keys();

    let mut prompt_len = prompt(&mut app)?;

    let (mut x, mut y) = app.term.cursor_pos()?;

    while let Some(Ok(key)) = input.next() {
        match key {
            Key::Char('\t') => {
                handle_autocomplete(&mut app)?;
                continue;
            }
            Key::Char('\n') if app.buf.left.last() != Some(&'\\') => {
                app.history.reset_index();
                reset_cursor_and_clear(&mut app, x, y)?;
                handle_exec(&mut app)?;
                // sleep a tiny amount to avoid messing out term output on quick commands that run
                // on the background, otherwise prompt can get rendered in between
                thread::sleep(Duration::from_millis(20));
                prompt_len = prompt(&mut app)?;
                (x, y) = app.term.cursor_pos()?;
                (curr_x, curr_y) = (x, y);
            }
            Key::Char(c) => {
                handle_new_char(&mut app, c)?;
                reset_cursor_and_clear(&mut app, x, y)?;
                display_bufs(&mut app, x, y)?;
                let (max_x, max_y) = terminal_size()?;

                if curr_y == max_y {
                    if c == '\n' {
                        y -= 1;
                    } else {
                        let second_last =
                            app.buf.left.len().checked_sub(2).map(|i| app.buf.left[i]);
                        let (tail_chars, total_lines) = get_buffer_shape(max_x, &app, prompt_len);
                        if tail_chars == 1 && total_lines > 1 && second_last != Some('\n') {
                            y -= 1
                        }
                    }
                }

                continue;
            }
            Key::Backspace => handle_backspace(&mut app),
            Key::Up => handle_move_up(&mut app),
            Key::Down => handle_move_down(&mut app),
            Key::Left => handle_move_left(&mut app),
            Key::ShiftRight => handle_move_shift_right(&mut app),
            Key::ShiftLeft => handle_move_shift_left(&mut app),
            Key::Right => handle_move_right(&mut app),
            Key::Ctrl('F') => handle_fuzzy_find(&mut app, &mut input)?,
            Key::Ctrl('f') => handle_fuzzy_find(&mut app, &mut input)?,
            Key::Ctrl('c') => {
                handle_ctrlc(&mut app, 1, y)?;
                prompt(&mut app)?;
                (x, y) = app.term.cursor_pos()?;
                (curr_x, curr_y) = (x, y);
            }
            _ => {}
        }
        display_bufs(&mut app, x, y)?;
    }

    Ok(())
}

// returns how many characters are un the buffers tail (last line) and how many lines there are
// this is used to handle the cursor's height in the terminal
fn get_buffer_shape(max_x: u16, app: &AppState, prompt_len: usize) -> (usize, usize) {
    let mut total_lines = 1;
    let mut total_chars = 0;
    for c in app.buf.left.iter() {
        let max_line_len = if total_lines == 1 {
            max_x as usize - prompt_len
        } else {
            max_x as usize
        };
        if *c == '\n' {
            total_chars = 0;
            total_lines += 1;
        } else {
            total_chars += 1;
            if total_chars == max_line_len {
                total_chars = 0;
                total_lines += 1;
            }
        }
    }

    (total_chars, total_lines)
}

fn reset_cursor_and_clear(app: &mut AppState, x: u16, y: u16) -> Result<(), Error> {
    write!(app.term, "{}{}", cursor::Goto(x, y), clear::AfterCursor)
}

fn display_bufs(app: &mut AppState, start_x: u16, start_y: u16) -> Result<(), Error> {
    write!(
        app.term,
        "{}{}",
        cursor::Goto(start_x, start_y),
        clear::AfterCursor
    )?;
    for c in app.buf.left.iter() {
        if *c == '\n' {
            write!(app.term, "\r")?;
        }
        write!(app.term, "{}", c)?;
    }

    write!(app.term, "{}", utils::cursor::Save)?;
    for c in app.buf.right.iter().rev() {
        if *c == '\n' {
            write!(app.term, "\r")?;
        }
        write!(app.term, "{}", c)?;
    }

    write!(app.term, "{}", utils::cursor::Restore)?;
    app.term.flush()
}

fn exec_tree(tree: &[parser::Node], ctx: &mut AppState) -> Result<nix::unistd::Pid, Error> {
    exec_program(tree, exec::StdChannels::default(), ctx)
}

fn handle_new_char(app: &mut AppState, c: char) -> Result<(), Error> {
    app.buf.left.push(c);
    Ok(())
}

fn handle_backspace(app: &mut AppState) {
    app.buf.left.pop();
}

fn handle_move_up(app: &mut AppState) {
    let Some(prev) = app.history.get_prev() else {
        return;
    };
    app.buf.left.clear();
    app.buf.right.clear();
    app.buf.left.extend(prev.chars());
}

fn handle_move_down(app: &mut AppState) {
    let Some(next) = app.history.get_next() else {
        return;
    };
    app.buf.left.clear();
    app.buf.right.clear();
    app.buf.left.extend(next.chars());
}

fn handle_move_left(app: &mut AppState) {
    app.buf.move_l2r();
}

fn handle_move_right(app: &mut AppState) {
    app.buf.move_r2l();
}

fn handle_move_shift_left(app: &mut AppState) {
    while let Some(c) = app.buf.left.pop() {
        app.buf.right.push(c);
        if c == ' ' {
            break;
        }
    }
}

fn handle_move_shift_right(app: &mut AppState) {
    while let Some(c) = app.buf.right.pop() {
        app.buf.left.push(c);
        if c == ' ' {
            break;
        }
    }
}

fn handle_ctrlc(app: &mut AppState, x: u16, y: u16) -> Result<(), Error> {
    app.buf.left.clear();
    app.buf.right.clear();
    reset_cursor_and_clear(app, x, y)?;
    app.term.flush()?;
    Ok(())
}

fn handle_exec(app: &mut AppState) -> Result<(), Error> {
    if app.buf.is_empty() {
        write_and_flush(&mut app.term, &format!("{}\r❯\r\n", clear::CurrentLine))?;
        return Ok(());
    }

    let text = app.buf.string_nc();

    app.buf.left.clear();
    app.buf.right.clear();
    app.history.set_current(app.buf.string_nc());
    write_and_flush(
        &mut app.term,
        &format!(
            "{}\r❯ {}\r\n",
            clear::CurrentLine,
            &text.replace("\n", "\r\n").trim()
        ),
    )?;

    app.term.suspend_raw_mode()?;
    let tokens = Tokenizer::new(&text).collect::<Vec<_>>();
    let program = parser::generate_program(tokens.iter().peekable());
    match exec_tree(&program, app) {
        Ok(pid) => {
            let _ = pid.wait_for_or_interrupt(|_| true);
        }
        Err(e) => eprintln!("{APP_NAME_SHORT}: {e}"),
    }

    app.history.push(text);
    app.term.activate_raw_mode()?;
    Ok(())
}

fn handle_custom_suggestions(
    app: &mut AppState,
    leading: &str,
    cmd: &str,
    tip: &str,
    mut suggestions: Vec<String>,
) -> Result<(), Error> {
    suggestions.retain(|s| s.starts_with(tip));
    match suggestions.len() {
        0 => {}
        1 => {
            let completion = &suggestions[0];
            let current = cmd_from_parts(leading, cmd, completion);
            app.buf.left = current.chars().collect();
            write!(app.term, "\r{}", clear::AfterCursor)?;
            prompt(app)?;
        }
        _ => {
            if let Some(common) = get_common_substring(&suggestions) {
                let current = cmd_from_parts(leading, cmd, &common);
                app.buf.left = current.chars().collect();
            }
            write!(app.term, "\r{}\n", clear::AfterCursor)?;
            write!(app.term, "{}", suggestions.join("\n"))?;
            write!(app.term, "{}", cursor::Up(suggestions.len() as u16))?;
            prompt(app)?;
        }
    }

    app.term.activate_raw_mode()
}

fn handle_autocomplete(app: &mut AppState) -> Result<(), Error> {
    app.term.suspend_raw_mode()?;
    let current = app.buf.left.iter().collect::<String>();
    let (start, rest) = split_last_unescaped(&current, utils::is_control_flow);
    let (cmd, tip) = split_last_unescaped(rest, utils::is_whitespace);
    let tip = expand_home_symbol(tip.trim_start());
    let tip = remove_escape_codes(&tip);

    if let Some(suggestions) = autocomp::suggest_autocomp(rest) {
        return handle_custom_suggestions(app, start, cmd, &tip, suggestions);
    }

    let Some(dirs) = Navigator::list_or_parent(&tip) else {
        return Ok(());
    };

    let targets: Vec<_> = dirs
        .into_iter()
        .flatten()
        .filter(|entry| is_prefixed_with(&tip, entry))
        .collect();

    match targets.len() {
        0 => {}
        1 => {
            let completion = &targets[0];
            let current = cmd_from_parts(
                start,
                cmd,
                &add_escape_codes(&entry_to_path_str(completion)),
            );
            app.buf.left = current.chars().collect();
            app.history.set_current(current);
            write!(app.term, "\r{}", clear::AfterCursor)?;
            prompt(app)?;
        }
        _ => {
            if let Some(common) = get_dir_common_substring(targets.as_slice()) {
                let current = cmd_from_parts(start, cmd, &add_escape_codes(&common));
                app.buf.left = current.chars().collect();
            }
            let fmt_dirs = get_formatted_dirs(targets.as_slice(), terminal_size()?.0, None);
            let lines = fmt_dirs.len() as u16;
            write!(app.term, "\r{}\n", clear::AfterCursor)?;
            write!(app.term, "{}", fmt_dirs.join("\n"))?;
            write!(app.term, "{}", cursor::Up(lines as u16))?;
            prompt(app)?;
        }
    }
    app.term.activate_raw_mode()
}

fn handle_fuzzy_find(app: &mut AppState, keys: &mut Keys<Stdin>) -> Result<(), Error> {
    write_and_flush(&mut app.term, &format!("{}", ToAlternateScreen))?;
    let mut new_buffer: Option<String> = None;
    let cmds = app.history.all();
    display_cmd_history(&mut app.term, cmds)?;

    let mut selected = cmds.len().saturating_sub(1);
    let mut search = CharBuffer::new();
    while let Some(Ok(k)) = keys.next() {
        match k {
            Key::Esc | Key::Ctrl('c') => break,
            Key::Char('\n') => {
                let filtered_cmds = fuzzy_sort_strings(&search.string_nc(), cmds);
                if let Some(cmd) = filtered_cmds.get(selected) {
                    new_buffer = Some(cmd.0.to_string());
                }
                break;
            }
            Key::Char(c) => search.push(c),
            Key::Backspace => {
                search.left.pop();
            }
            Key::Left => {
                search.move_l2r();
            }
            Key::Right => {
                search.move_r2l();
            }
            Key::Up => {
                selected = selected.saturating_sub(1);
            }
            Key::Down => {
                selected = selected.saturating_add(1);
            }
            _ => {}
        }
        let needle = search.string_nc();
        let mut sorted = fuzzy_sort_strings(&needle, cmds);
        selected = selected.clamp(0, sorted.len().saturating_sub(1));
        let _ = sorted
            .get_mut(selected)
            .map(|matched| matched.set_selected());
        display_cmd_history(&mut app.term, &sorted)?;
        write_and_flush(&mut app.term, &search)?;
    }

    write_and_flush(&mut app.term, &format!("{}", ToMainScreen))?;
    if let Some(new_buffer) = new_buffer {
        app.buf.left.clear();
        app.buf.right.clear();
        app.buf.left = new_buffer.chars().collect();
        prompt(app)?;
    }

    Ok(())
}
