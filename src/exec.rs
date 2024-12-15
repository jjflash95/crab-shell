use std::{
    fs::{File, OpenOptions},
    io::{self, stderr, stdout, Error, ErrorKind, Read as _, Stderr, Stdout, Write},
    os::fd::{AsRawFd, FromRawFd as _},
    process::{Command, Stdio},
};

use glob::glob;
use itertools::Itertools as _;
use nix::{
    sys::{
        signal::Signal,
        wait::{waitpid, WaitStatus},
    },
    unistd::{dup, fork, ForkResult, Pid},
};

use crate::{
    app::{AppState, Sourcer},
    expand::{lexer::Token as ArgToken, tokenize},
    lexer::Tokenizer,
    parser::{ast, Cmd, Expr, FileMode, Node, Stmt},
    utils, APP_NAME_SHORT,
};

pub const NOOP_PROGRAM: Cmd<'static> = Cmd {
    program: "true",
    args: vec![],
};

pub const BREAK_PROGRAM: Cmd<'static> = Cmd {
    program: "break",
    args: vec![],
};

pub struct StdChannels<I, O, E>(pub I, pub O, pub E);

impl<I, O, E> StdChannels<I, O, E> {
    fn inner(self) -> (I, O, E) {
        (self.0, self.1, self.2)
    }
}

impl<I, O, E> StdChannels<I, O, E>
where
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    // Creates a new set of channels with
    // - inherited stdin (stdin cannot be copied as a fd, but thankfully we don't need it)
    // - copied stdout from the original channel
    // - copied stderr from the original channel
    //
    // This allows use to reuse descriptors between multiple processes even after consuming them
    fn dup(&self) -> Result<StdChannels<Stdio, File, File>, Error> {
        let out = clone(&self.1)?;
        let err = clone(&self.2)?;
        let din = Stdio::inherit();
        Ok(StdChannels(din, out, err))
    }
}

impl Default for StdChannels<Stdio, Stdout, Stderr> {
    fn default() -> Self {
        StdChannels(Stdio::inherit(), std::io::stdout(), std::io::stderr())
    }
}

impl<I: Into<Stdio> + AsRawFd, O: Into<Stdio>, E: Into<Stdio>> From<StdChannels<I, O, E>>
    for (I, O, E)
{
    fn from(channels: StdChannels<I, O, E>) -> Self {
        let StdChannels(stdin, stdout, stderr) = channels;
        (stdin, stdout, stderr)
    }
}

trait ErrWithCtx {
    fn add_ctx(self, program: &str) -> Self;
}

pub trait WaitableProcess {
    // Wait for process to end and don't fail on manual interrupt
    fn wait_for_or_interrupt(&self, cond: impl Fn(i32) -> bool) -> Result<(), Error>;
    // Wait for process to end with status code
    fn wait_for(&self, cond: impl Fn(i32) -> bool) -> Result<(), Error>;
}

impl<T> ErrWithCtx for Result<T, Error> {
    fn add_ctx(self, program: &str) -> Self {
        self.map_err(|e| Error::new(e.kind(), format!("{}: {}", e.kind(), program)))
    }
}

impl WaitableProcess for Pid {
    fn wait_for_or_interrupt(&self, cond: impl Fn(i32) -> bool) -> Result<(), Error> {
        match self.wait_for(cond) {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == ErrorKind::Interrupted => Ok(()),
            e => e,
        }
    }

    fn wait_for(&self, cond: impl Fn(i32) -> bool) -> Result<(), Error> {
        use WaitStatus::*;

        loop {
            match waitpid(*self, Some(nix::sys::wait::WaitPidFlag::WNOHANG)) {
                Ok(Exited(_pid, c)) if cond(c) => return Ok(()),
                Ok(Exited(_pid, c)) if !cond(c) => return Err(Error::from(ErrorKind::Other)),
                Ok(Signaled(_pid, Signal::SIGINT, _)) => {
                    return Err(Error::from(ErrorKind::Interrupted))
                }
                Ok(StillAlive) => continue,
                Ok(e) => return Err(Error::new(ErrorKind::Other, format!("{e:?}"))),
                Err(e) => {
                    return Err(Error::from_raw_os_error(e as i32));
                }
            }
        }
    }
}

pub fn exec_program<I, E, O>(
    nodes: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    let (nodes, tail) = match nodes {
        [] => return Err(Error::new(ErrorKind::Other, "empty program")),
        [nodes @ .., tail] => (nodes, tail),
    };

    for node in nodes {
        exec_node(node, channels.dup()?, ctx)?.wait_for(|s| s == 0)?;
    }

    exec_node(tail, channels, ctx)
}

pub fn exec_node<I, E, O>(
    node: &Node,
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    match node {
        Node::Statement(stmt) => exec_stmt(stmt, channels, ctx),
        Node::Expression(expr) => exec_ast(expr, channels, ctx),
    }
}

pub fn exec_stmt<I, E, O>(
    stmt: &Stmt,
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    use Stmt::*;

    match stmt {
        If {
            cond,
            then,
            or_then,
        } => exec_if(cond, then, or_then.as_deref(), channels, ctx),
        For { var, items, body } => exec_for(var, items, body, channels, ctx),
        While { cond, body } => exec_while(cond, body, channels, ctx),
        Block { ops: nodes, _async } => {
            if *_async {
                return exec_block_forked(nodes, channels, ctx);
            }
            exec_block(nodes, channels, ctx)
        }
    }
}

pub fn exec_if<I, E, O>(
    cond: &Expr,
    then: &Node,
    or_else: Option<&Node>,
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    if exec_ast(cond, channels.dup()?, ctx)?
        .wait_for(|c| c == 0)
        .is_ok()
    {
        exec_node(then, channels, ctx)
    } else if let Some(or_then) = or_else {
        exec_node(or_then, channels, ctx)
    } else {
        exec_noop(channels, ctx)
    }
}

pub fn exec_for<I, E, O>(
    var: &str,
    items: &[&str],
    body: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    ctx.enable_breaker();
    let res = exec_for_inner(var, items, body, channels, ctx);
    ctx.disable_breaker();
    res
}

pub fn exec_for_inner<I, E, O>(
    var: &str,
    items: &[&str],
    body: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    for item in items {
        if ctx.should_break() {
            break;
        }
        let item = evaluate_arg(item, ctx);
        ctx.locals.insert(var.to_string(), item);
        exec_block(body, channels.dup()?, ctx)?;
    }

    exec_noop(channels, ctx)
}

pub fn exec_while<I, E, O>(
    cond: &Expr,
    body: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    ctx.enable_breaker();
    let res = exec_while_inner(cond, body, channels, ctx);
    ctx.disable_breaker();
    res
}

pub fn exec_while_inner<I, E, O>(
    cond: &Expr,
    body: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    while !ctx.should_break()
        && exec_ast(cond, channels.dup()?, ctx)
            .and_then(|child| child.wait_for(|c| c == 0))
            .is_ok()
    {
        exec_block(body, channels.dup()?, ctx)?;
    }
    exec_noop(channels, ctx)
}

pub fn exec_block_forked<I, E, O>(
    nodes: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    match unsafe { fork() }? {
        ForkResult::Child => {
            let StdChannels(_, out, err) = channels;
            let mut outd = clone(&out).expect("failed to clone stdout");
            let channels = StdChannels(Stdio::piped(), out, err);
            write!(outd, "\r[{}]\r\n", std::process::id())?;
            let _ = exec_program(nodes, channels, ctx).and_then(|pid| pid.wait_for(|_| true));
            write!(outd, "\r[{}] + done {{ ", std::process::id())?;
            for (i, node) in nodes.iter().enumerate() {
                if i > 0 {
                    write!(outd, ";\r\n")?;
                }
                write!(outd, "{}", node)?;
            }
            write!(outd, " }}\r\n")?;
            std::process::exit(0);
        }
        ForkResult::Parent { .. } => exec_noop(channels, ctx),
    }
}

pub fn exec_block<I, E, O>(
    nodes: &[Node],
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    exec_program(nodes, channels, ctx)
}

pub fn exec_ast<I, E, O>(
    ast: &Expr,
    channels: StdChannels<I, E, O>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    use Expr::*;

    match ast {
        Cmd { op: cmd, _async } => {
            if *_async {
                return exec_cmd_forked(cmd, channels, ctx);
            }
            exec_cmd(cmd, channels, ctx)
        }
        Grouped { op, _async } => {
            if *_async {
                return exec_ast_forked(ast, channels, ctx);
            };
            exec_ast(op, channels, ctx)
        }
        Subshell { op: ss } => exec_subshell(ss, channels, ctx),
        Pipe { l, r } => exec_pipe(l, r, channels, ctx),
        And { l, r } => exec_and(l, r, channels, ctx),
        Or { l, r } => exec_or(l, r, channels, ctx),
        RedirOutput { op, mode, dest } => exec_redir_out(op, dest, mode, channels, ctx),
        RedirInput { op, src } => exec_redir_inp(op, src, channels, ctx),
    }
}

fn exec_pipe<I, O, E>(
    l: &Expr,
    r: &Expr,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    // get piped descriptors, use write for p1::stdout and read for p2::stdin
    // for the other pipes we just nix::unixstd::dup() the given channels
    let (pr, pw) = nix::unistd::pipe()?;
    let stdout = File::from(pw);
    let stdin = File::from(pr);
    let (cin, out, err) = channels.inner();
    let just_in_case = err.as_raw_fd();
    let channels = StdChannels(stdin, out, clone(&err)?);
    let cmd = exec_ast(l, StdChannels(cin, stdout, err), ctx);
    if let Err(e) = cmd.as_ref() {
        let mut stderr = clone(&just_in_case)?;
        write!(stderr, "internal: {}\r\n", e.kind())?;
    }
    let _ = exec_ast(r, channels, ctx)?.wait_for(|_| true); // wait here to avoid turning off raw
                                                            // mode when first command ends
    cmd
}

fn exec_and<I, O, E>(
    l: &Expr,
    r: &Expr,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    let fb_channels = channels.dup()?;
    let child = exec_ast(l, channels, ctx)?;
    child.wait_for(|c| c == 0)?;
    exec_ast(r, fb_channels, ctx)
}

fn exec_or<I, O, E>(
    l: &Expr,
    r: &Expr,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    match exec_ast(l, channels.dup()?, ctx) {
        Ok(child) => {
            if child.wait_for(|c| c == 0).is_ok() {
                // HACK: since previous command has already been awaited, returning the previous
                // commands Pid and waiting for it will error with ECHILD, so instead we create a
                // noop and return its exit code
                return exec_noop(channels, ctx);
            }
        }
        Err(e) => {
            let StdChannels(_, _, err) = &channels;
            let mut stderr = clone(&err.as_raw_fd())?;
            stderr.write_all(format!("{}: internal: {}\r\n", APP_NAME_SHORT, e).as_bytes())?;
            stderr.flush()?;
        }
    };

    exec_ast(r, channels, ctx)
}

fn exec_redir_out<I, O, E>(
    expr: &Expr,
    dest: &str,
    mode: &FileMode,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    use FileMode::*;

    let out = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(*mode == Truncate)
        .append(*mode == Append)
        .open(dest)?;

    let channels = StdChannels(channels.0, out, channels.2);
    exec_ast(expr, channels, ctx)
}

fn exec_redir_inp<I, O, E>(
    expr: &Expr,
    src: &str,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    let file = OpenOptions::new().read(true).write(false).open(src)?;
    let channels = StdChannels(file, channels.1, channels.2);
    exec_ast(expr, channels, ctx)
}

fn exec_subshell<I, O, E>(
    ss: &Expr,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    match unsafe { fork()? } {
        ForkResult::Child => {
            let _ = exec_ast(ss, channels, ctx);
            std::process::exit(0);
        }
        ForkResult::Parent { child } => Ok(child),
    }
}

fn exec_ast_forked<I, O, E>(
    ast: &Expr,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    match unsafe { fork() }? {
        ForkResult::Child => {
            let StdChannels(_, out, err) = channels;
            let mut outd = clone(&out).expect("failed to clone stdout");
            let channels = StdChannels(Stdio::piped(), out, err);
            print!("\r[{}]\r\n", std::process::id());
            let _ = exec_ast(ast, channels, ctx).and_then(|pid| pid.wait_for(|_| true));
            write!(outd, "\r[{}] + done {}\r\n", std::process::id(), ast)?;
            std::process::exit(0);
        }
        ForkResult::Parent { .. } => exec_noop(channels, ctx),
    }
}

fn exec_cmd_forked<I, O, E>(
    cmd: &Cmd,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    match unsafe { fork() }? {
        ForkResult::Child => {
            let StdChannels(_, out, err) = channels;
            let mut outd = clone(&out).expect("failed to clone stdout");
            let channels = StdChannels(Stdio::piped(), out, err);
            ctx.term.suspend_raw_mode()?;
            write!(outd, "\r[{}]\r\n", std::process::id())?;
            let _ = exec_cmd(cmd, channels, ctx).and_then(|pid| pid.wait_for(|_| true));
            write!(outd, "\r[{}] + done {}\r\n", std::process::id(), cmd)?;
            ctx.term.activate_raw_mode()?;
            std::process::exit(0);
        }
        ForkResult::Parent { .. } => exec_noop(channels, ctx),
    }
}

fn exec_cmd<I, O, E>(
    cmd: &Cmd,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    // this is the least invasive way I found to clear the prompt cursor
    // when executing commands in background so that the prompt does not step
    // over the commands output
    write!(ctx.term, "{}\r", termion::clear::CurrentLine)?;
    let _ = ctx.term.flush();

    match cmd.program {
        "git" => {
            exec_cmd_inner(cmd.program, &cmd.args, channels.dup()?, ctx)?.wait_for(|c| c == 0)?;
            ctx.reset_branch();
            exec_noop(channels, ctx)
        }
        "cd" => {
            std::env::set_current_dir(cmd.args.first().copied().unwrap_or("/"))?;
            ctx.reset_branch();
            exec_noop(channels, ctx)
        }
        "export" => {
            set_vars(&cmd.args, true, channels.dup()?, ctx)?;
            exec_noop(channels, ctx)
        }
        "set" => {
            set_vars(&cmd.args, false, channels.dup()?, ctx)?;
            exec_noop(channels, ctx)
        }
        "source" | "." => {
            source(&cmd.args, ctx)?;
            exec_noop(channels, ctx)
        }
        "history" => {
            if cmd.args.iter().any(|a| *a == "--save" || *a == "-s") {
                ctx.history.save()?;
            }
            if cmd.args.iter().any(|a| *a == "--reload" || *a == "-r") {
                ctx.history.reload()?;
            }
            if cmd.args.is_empty() {
                write!(
                    clone(&channels.1)?,
                    "{}\r\n",
                    ctx.history
                        .src()
                        .map(|p| p.as_os_str().to_str().unwrap_or_default())
                        .unwrap_or_default()
                )?;
            }
            exec_noop(channels, ctx)
        }
        "colorscheme" => {
            for arg in &cmd.args {
                match *arg {
                    "default" => {
                        Sourcer::source_from_text(include_str!("../themes/default.cs"), ctx)
                    }
                    "blue" => Sourcer::source_from_text(include_str!("../themes/blue.cs"), ctx),
                    "sunset" => Sourcer::source_from_text(include_str!("../themes/sunset.cs"), ctx),
                    "green" => Sourcer::source_from_text(include_str!("../themes/sunset.cs"), ctx),
                    "dark" => Sourcer::source_from_text(include_str!("../themes/dark.cs"), ctx),
                    "boke" => Sourcer::source_from_text(include_str!("../themes/boke.cs"), ctx),
                    _ => source(&[arg], ctx)?,
                }
            }

            exec_noop(channels, ctx)
        }
        "break" => {
            if !ctx.toggle_breaker() {
                return Err(Error::new(ErrorKind::Other, "break called outside loop"));
            };
            exec_noop(channels, ctx)
        }
        "exit" => std::process::exit(0),
        program => exec_cmd_inner(program, &cmd.args, channels, ctx),
    }
}

fn exec_noop<I, O, E>(channels: StdChannels<I, O, E>, ctx: &mut AppState) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    exec_cmd(&NOOP_PROGRAM, channels, ctx)
}

fn exec_cmd_inner<I, O, E>(
    program: &str,
    args: &[&str],
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<Pid, Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    let (stdin, stdout, stderr) = channels.inner();

    let args: Vec<_> = args
        .iter()
        .map(|s| utils::remove_escape_codes(s))
        .map(|s| evaluate_arg(&s, ctx))
        .flat_map(expand_wildcards)
        .collect();

    let child = Command::new(program)
        .args(args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .add_ctx(program)?;

    Ok(Pid::from_raw(child.id() as i32))
}

fn evaluate_arg(arg: &str, ctx: &mut AppState) -> String {
    tokenize(arg)
        .iter()
        .map(|t| expand(t, ctx))
        .map(|e| {
            e.inspect_err(|e| eprintln!("subcommand: {e}"))
                .unwrap_or_default()
        })
        .collect::<Vec<_>>()
        .join("")
}

fn expand(token: &ArgToken, ctx: &mut AppState) -> Result<String, Error> {
    use ArgToken::*;

    match token {
        Whitespace(ws) => Ok(ws.to_string()),
        Word(word) => Ok(word.to_string()),
        Variable(name) => Ok(ctx.get_var(name).unwrap_or_default().to_owned()),
        Quoted(s) => Ok(evaluate_arg(s, ctx)),
        CmdExpansion(cmd) => parse_exec_and_output(cmd, ctx),
        VariableExpr(expr) => Ok(ctx.get_var(expr).unwrap_or_default().to_owned()), // TODO: make replacements, i.e.
                                                                                    // VAR=hello ${VAR/o/O} => hellO
    }
}

fn expand_wildcards(text: String) -> Vec<String> {
    let Ok(paths) = glob(&text) else {
        return vec![text];
    };

    let paths = paths
        .into_iter()
        .flatten()
        .filter_map(|p| p.to_str().map(str::to_string))
        .filter(|p| p != "./." && p != "./..")
        .collect::<Vec<_>>();

    if paths.is_empty() {
        vec![text]
    } else {
        paths
    }
}

fn parse_exec_and_output(cmd: &str, ctx: &mut AppState) -> Result<String, Error> {
    let tokenized = Tokenizer::new(cmd).collect::<Vec<_>>();
    let tokens = tokenized.iter().peekable();
    let (_, tree) = ast(tokens).map_err(|e| Error::new(ErrorKind::InvalidInput, e))?;

    let (pr, pw) = nix::unistd::pipe()?;
    let read = File::from(pr);
    let write = File::from(pw);
    let stdin = Stdio::inherit();
    let stderr = stderr();

    let pid = exec_ast(&tree, StdChannels(stdin, write, stderr), ctx)?;
    pid.wait_for(|_| true)?;

    let mut output = String::new();
    let mut reader = io::BufReader::new(read);
    reader.read_to_string(&mut output)?;
    if output.ends_with('\n') {
        output.pop();
    }
    Ok(output)
}

fn show_vars<V, K, I, O, E>(vars: V, mut channels: StdChannels<I, O, E>) -> Result<(), Error>
where
    K: AsRef<str>,
    V: Iterator<Item = (K, K)>,
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    for (key, val) in vars {
        write!(&mut channels.1, "{}={}\r\n", key.as_ref(), val.as_ref())?;
    }
    Ok(())
}

fn source(args: &[&str], ctx: &mut AppState) -> Result<(), Error> {
    for arg in args {
        let file = evaluate_arg(arg, ctx);
        if let Err(e) = Sourcer::source_from_file(&file, ctx) {
            eprintln!("failed sourcing {file}: {e}");
        }
    }

    Ok(())
}

fn set_vars<I, O, E>(
    args: &[&str],
    export: bool,
    channels: StdChannels<I, O, E>,
    ctx: &mut AppState,
) -> Result<(), Error>
where
    I: Into<Stdio>,
    O: Into<Stdio> + AsRawFd + Write,
    E: Into<Stdio> + AsRawFd + Write,
{
    if args.is_empty() {
        return if export {
            show_vars(std::env::vars(), channels)
        } else {
            show_vars(ctx.locals.iter(), channels)
        };
    }

    for arg in args {
        let Some((name, value)) = arg.split_once('=') else {
            return Err(Error::new(
                ErrorKind::InvalidInput,
                format!("wrongly formatted argument {arg}"),
            ));
        };

        let name = evaluate_arg(name, ctx);
        let value = evaluate_arg(value, ctx);
        if !name.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(Error::new(
                ErrorKind::InvalidInput,
                format!("wrongly formatted varname, chars must be alphanum or '_': {name}"),
            ));
        }
        if export {
            std::env::set_var(name.as_str(), value.as_str());
        }
        ctx.locals.insert(name, value.to_string());
    }
    Ok(())
}

fn clone<C: AsRawFd>(channel: &C) -> Result<File, Error> {
    Ok(unsafe { File::from_raw_fd(dup(channel.as_raw_fd())?) })
}
