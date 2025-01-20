use std::{fmt::Display, iter::Peekable, slice::Iter};

use crate::lang::lexer::{RedirOp, Token};

macro_rules! try_parsers {
    ($input:expr, $first:expr $(, $rest:expr)*) => {{
        $first($input.clone()) $(.or_else(|_| $rest($input.clone())))*
    }};
}

type ParseRes<'a> = Result<(TokenStream<'a>, Expr<'a>), String>;

type TokenStream<'a> = Peekable<Iter<'a, Token<'a>>>;

#[derive(Debug, PartialEq)]
pub enum FileMode {
    Truncate,
    Append,
}

#[derive(Debug, PartialEq)]
pub struct Cmd<'a> {
    pub program: &'a str,
    pub args: Vec<&'a str>,
}

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Statement(Stmt<'a>),
    Expression(Expr<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    If {
        cond: Box<Expr<'a>>,
        then: Box<Node<'a>>,
        or_then: Option<Box<Node<'a>>>,
    },
    While {
        cond: Box<Expr<'a>>,
        body: Vec<Node<'a>>,
    },
    For {
        var: &'a str,
        items: Vec<&'a str>,
        body: Vec<Node<'a>>,
    },
    Block {
        ops: Vec<Node<'a>>,
        _async: bool,
    },
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    And {
        l: Box<Expr<'a>>,
        r: Box<Expr<'a>>,
    },
    Or {
        l: Box<Expr<'a>>,
        r: Box<Expr<'a>>,
    },
    RedirOutput {
        mode: FileMode,
        op: Box<Expr<'a>>,
        dest: &'a str,
    },
    RedirInput {
        op: Box<Expr<'a>>,
        src: &'a str,
    },
    Pipe {
        l: Box<Expr<'a>>,
        r: Box<Expr<'a>>,
    },
    Subshell {
        op: Box<Expr<'a>>,
    },
    Grouped {
        op: Box<Expr<'a>>,
        _async: bool,
    },
    Cmd {
        op: Cmd<'a>,
        _async: bool,
    },
}

trait ExpectableIter {
    type Item;

    fn expect_next<F: Fn(&Token<'_>) -> bool>(
        &mut self,
        predicate: F,
    ) -> Result<Self::Item, String>;
}

impl<'a> ExpectableIter for TokenStream<'a> {
    type Item = Token<'a>;

    fn expect_next<F: Fn(&Token<'_>) -> bool>(
        &mut self,
        predicate: F,
    ) -> Result<Self::Item, String> {
        match self.peek() {
            Some(t) if predicate(t) => Ok(*self.next().unwrap()),
            Some(t) if **t == Token::Newline => {
                self.next();
                self.expect_next(predicate)
            }
            Some(t) => Err(format!("Unexpected token {t:?}")),
            None => Err("Unexpected EOT".to_string()),
        }
    }
}

pub fn node(mut source: TokenStream) -> Result<(TokenStream, Node), String> {
    match source.peek() {
        Some(Token::Newline) => {
            source.next();
            node(source)
        }
        Some(Token::If) => {
            let (source, stmt) = if_node(source)?;
            Ok((source, Node::Statement(stmt)))
        }
        Some(Token::While) => {
            let (source, stmt) = while_node(source)?;
            Ok((source, Node::Statement(stmt)))
        }
        Some(Token::For) => {
            let (source, stmt) = for_node(source)?;
            Ok((source, Node::Statement(stmt)))
        }
        Some(Token::LBrace) => {
            let (source, stmt) = block_node(source)?;
            Ok((source, Node::Statement(stmt)))
        }
        _ => {
            // If it's not a statement, try parsing it as an expression
            let (source, expr) = ast(source)?;
            Ok((source, Node::Expression(expr)))
        }
    }
}

#[allow(dead_code)]
fn pattern_match_predicate(source: TokenStream) -> Result<(TokenStream, Expr), String> {
    // TODO: parse pattern match
    test_cmd_prefix(source)
}

fn test_cmd_prefix(mut source: TokenStream) -> Result<(TokenStream, Expr), String> {
    source.expect_next(|t| matches!(t, Token::LBracket))?;
    let (
        mut source,
        Expr::Cmd {
            op: Cmd { program, args },
            _async,
        },
    ) = cmd(source)?
    else {
        Err("expected command".to_string())?
    };

    source.expect_next(|t| matches!(t, Token::RBracket))?;
    let mut exp_args = vec![program];
    exp_args.extend(args);

    Ok((
        source,
        Expr::Cmd {
            op: Cmd {
                program: "test",
                args: exp_args,
            },
            _async,
        },
    ))
}

fn if_node(mut source: TokenStream) -> Result<(TokenStream, Stmt), String> {
    source.expect_next(|t| matches!(t, Token::If))?;

    let (mut source, cond) = if source.peek() == Some(&&Token::LBracket) {
        test_cmd_prefix(source)?
    } else {
        ast(source)?
    };

    let _ = source.expect_next(|t| matches!(t, Token::Semicolon));
    source.expect_next(|t| matches!(t, Token::Then))?;
    let (mut source, then) = node(source)?;
    let _ = source.expect_next(|t| matches!(t, Token::Semicolon));

    let (mut source, or_then) = if source.peek() == Some(&&Token::Else) {
        source.next();
        let (source, else_stmt) = node(source)?;
        (source, Some(Box::new(else_stmt)))
    } else {
        (source, None)
    };

    source.expect_next(|t| matches!(t, Token::Endif))?;

    Ok((
        source,
        Stmt::If {
            cond: cond.into(),
            then: then.into(),
            or_then,
        },
    ))
}

fn while_node(mut source: TokenStream) -> Result<(TokenStream, Stmt), String> {
    source.expect_next(|t| matches!(t, Token::While))?;
    let (mut source, cond) = ast(source)?;
    let _ = source.expect_next(|t| matches!(t, Token::Semicolon))?;
    source.expect_next(|t| matches!(t, Token::Do))?;

    let mut body = vec![];
    while let Ok(res) = node(source.clone()) {
        let (s, n) = res;
        source = s;
        body.push(n);
    }

    source.expect_next(|t| matches!(t, Token::Done))?;

    Ok((
        source,
        Stmt::While {
            cond: cond.into(),
            body,
        },
    ))
}

fn for_node(mut source: TokenStream) -> Result<(TokenStream, Stmt), String> {
    source.expect_next(|t| matches!(t, Token::For))?;
    let var = source
        .expect_next(|t| matches!(t, Token::Word(_)))?
        .into_str();

    source.expect_next(|t| matches!(t, Token::In))?;
    let first = source
        .expect_next(|t| matches!(t, Token::Word(_)))?
        .into_str();

    let mut items = vec![first];
    while let Some(Token::Word(_)) = source.peek() {
        items.push(source.next().unwrap().into_str());
    }
    let _ = source.expect_next(|t| matches!(t, Token::Semicolon));
    source.expect_next(|t| matches!(t, Token::Do))?;
    let mut body = vec![];

    while let Ok(res) = node(source.clone()) {
        let (s, n) = res;
        source = s;
        body.push(n);
    }

    source.expect_next(|t| matches!(t, Token::Done))?;
    Ok((source, Stmt::For { var, items, body }))
}

fn block_node(mut source: TokenStream) -> Result<(TokenStream, Stmt), String> {
    source.expect_next(|t| matches!(t, Token::LBrace))?;
    let mut statements = Vec::new();

    while let Ok((new_source, stmt)) = node(source.clone()) {
        source = new_source;
        statements.push(stmt);
    }

    source.expect_next(|t| matches!(t, Token::RBrace))?;

    let _async = source
        .expect_next(|t| matches!(t, Token::Background))
        .map(|_| true)
        .unwrap_or_default();

    Ok((
        source,
        Stmt::Block {
            ops: statements,
            _async,
        },
    ))
}

pub fn ast(source: TokenStream) -> ParseRes {
    andor(source)
}

pub fn pipe(source: TokenStream) -> ParseRes {
    let (mut source, left) = redirect(source)?;
    if source.expect_next(|t| matches!(t, Token::Pipe)).is_ok() {
        let (source, right) = pipe(source)?;
        return Ok((
            source,
            Expr::Pipe {
                l: left.into(),
                r: right.into(),
            },
        ));
    };

    Ok((source, left))
}

pub fn redirect(source: TokenStream) -> ParseRes {
    let (mut source, left) = term(source)?;
    if let Ok(Token::Redirect(op)) = source.expect_next(|t| matches!(t, Token::Redirect(_))) {
        match op {
            RedirOp::Input => {
                let src = source.expect_next(|t| matches!(t, Token::Word(_)))?;
                let expr = Expr::RedirInput {
                    op: left.into(),
                    src: src.into_str(),
                };
                Ok((source, expr))
            }
            RedirOp::Output => {
                let dest = source.expect_next(|t| matches!(t, Token::Word(..)))?;
                let expr = Expr::RedirOutput {
                    op: left.into(),
                    dest: dest.into_str(),
                    mode: FileMode::Truncate,
                };
                Ok((source, expr))
            }
            RedirOp::Append => {
                let dest = source.expect_next(|t| matches!(t, Token::Word(..)))?;
                let expr = Expr::RedirOutput {
                    op: left.into(),
                    dest: dest.into_str(),
                    mode: FileMode::Append,
                };
                Ok((source, expr))
            }
        }
    } else {
        Ok((source, left))
    }
}

pub fn andor(source: TokenStream) -> ParseRes {
    let (mut source, left) = pipe(source)?;
    if let Ok(t) = source.expect_next(|t| matches!(t, Token::And | Token::Or)) {
        match t {
            Token::And => {
                let (source, right) = andor(source)?;
                let expr = Expr::And {
                    l: left.into(),
                    r: right.into(),
                };
                Ok((source, expr))
            }
            Token::Or => {
                let (source, right) = andor(source)?;
                let expr = Expr::Or {
                    l: left.into(),
                    r: right.into(),
                };
                Ok((source, expr))
            }
            _ => unreachable!(),
        }
    } else {
        Ok((source, left))
    }
}

pub fn term(source: TokenStream) -> ParseRes {
    try_parsers!(source, _break, test_cmd_prefix, set, grouped, subshell, cmd)
}

// This accomplishes the same as cmd(["break"]) but was removed for clarity
pub fn _break(mut source: TokenStream) -> ParseRes {
    source.expect_next(|t| matches!(t, Token::Word("break")))?;
    Ok((
        source,
        Expr::Cmd {
            op: crate::exec::BREAK_PROGRAM,
            _async: false,
        },
    ))
}

pub fn set(mut source: TokenStream) -> ParseRes {
    let text = source
        .expect_next(|t| matches!(t, Token::Word(_)))?
        .into_str();
    if text.split_once('=').is_none() {
        return Err(format!(
            "setvar format must be {{varname}}={{value}}, found {text} instead"
        ));
    }
    let cmd = Cmd {
        program: "set",
        args: vec![text],
    };
    Ok((
        source,
        Expr::Cmd {
            op: cmd,
            _async: false,
        },
    ))
}

pub fn grouped(mut source: TokenStream) -> ParseRes {
    let _ = source.expect_next(|t| matches!(t, Token::LBrace))?;
    let (mut source, expr) = ast(source)?;
    let _ = source.expect_next(|t| matches!(t, Token::RBrace))?;
    let _async = source
        .expect_next(|t| matches!(t, Token::Background))
        .map(|_| true)
        .unwrap_or_default();
    Ok((
        source,
        Expr::Grouped {
            op: expr.into(),
            _async,
        },
    ))
}

pub fn subshell(mut source: TokenStream) -> ParseRes {
    let _ = source.expect_next(|t| matches!(t, Token::LParen))?;
    let (mut source, expr) = ast(source)?;
    let _ = source.expect_next(|t| matches!(t, Token::RParen))?;
    let _async = source
        .expect_next(|t| matches!(t, Token::Background))
        .map(|_| true)
        .unwrap_or_default();
    Ok((source, Expr::Subshell { op: expr.into() }))
}

pub fn cmd(mut source: TokenStream) -> ParseRes {
    let program = source
        .expect_next(|t| matches!(t, Token::Word(_)))?
        .into_str();
    let mut args = vec![];
    loop {
        if !matches!(
            source.peek(),
            Some(Token::Word(_)) | Some(Token::QuotedStr(_))
        ) {
            break;
        }
        args.push(source.next().unwrap().into_str());
    }

    let _async = source
        .expect_next(|t| matches!(t, Token::Background))
        .map(|_| true)
        .unwrap_or_default();

    let expr = Expr::Cmd {
        op: Cmd { program, args },
        _async,
    };
    Ok((source, expr))
}

pub fn generate_program<'a>(mut tokens: Peekable<Iter<'a, Token<'a>>>) -> Vec<Node<'a>> {
    let mut program = vec![];
    while let Ok((mut rest, n)) = node(tokens) {
        program.push(n);
        if let Some(Token::Semicolon) = rest.peek() {
            rest.next();
        }
        tokens = rest;
    }

    program
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Statement(s) => s.fmt(f),
            Node::Expression(e) => e.fmt(f),
        }
    }
}

impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::If {
                cond,
                then,
                or_then,
            } => {
                write!(f, "if {}; then\r\n{}", cond, then)?;
                if let Some(or_then) = or_then {
                    write!(f, "\r\n{}", or_then)?;
                }
                write!(f, "\r\ndone")
            }
            Stmt::For { var, items, body } => {
                write!(f, "for {} in ", var)?;
                for item in items {
                    write!(f, " {}", item)?;
                }
                write!(f, "do;")?;
                for node in body {
                    write!(f, "\r\n{}", node)?;
                }
                write!(f, "\r\ndone")
            }
            Stmt::Block { ops: body, _async } => {
                write!(f, "{{")?;
                for node in body {
                    write!(f, "{}{}", if body.len() > 1 { "\r\n" } else { "" }, node)?;
                }
                write!(f, "{}}}", if body.len() > 1 { "\r\n" } else { "" })?;
                if *_async {
                    write!(f, " &")?;
                }
                Ok(())
            }
            Stmt::While { cond, body } => {
                write!(f, "while {} do;", cond)?;
                for node in body {
                    write!(f, "\r\n{}", node)?;
                }
                write!(f, "\r\ndone")
            }
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Cmd { op: cmd, _async } => {
                cmd.fmt(f)?;
                if *_async {
                    write!(f, " &")?;
                }
                Ok(())
            }
            Expr::RedirInput { op, src } => write!(f, "{} < {}", op, src),
            Expr::RedirOutput { mode, op, dest } => write!(
                f,
                "{} {} {}",
                op,
                if *mode == FileMode::Append { ">>" } else { ">" },
                dest
            ),
            Expr::Or { l, r } => write!(f, "{} || {}", l, r),
            Expr::And { l, r } => write!(f, "{} && {}", l, r),
            Expr::Pipe { l, r } => write!(f, "{} | {}", l, r),
            Expr::Subshell { op: ss } => write!(f, "( {} )", ss),
            Expr::Grouped { op: ss, _async } => {
                write!(f, "{{ {} }}", ss)?;
                if *_async {
                    write!(f, " &")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Cmd<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.program)?;
        for arg in &self.args {
            write!(f, " {}", arg)?;
        }
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Tokenizer;

    #[test]
    fn test_simple_command() {
        let cmd = "ls -la";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Cmd {
            op: Cmd {
                program: "ls",
                args: vec!["-la"],
            },
            _async: false,
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_and() {
        let cmd = "echo a && echo b";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::And {
            l: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["a"],
                },
                _async: false,
            }),
            r: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["b"],
                },
                _async: false,
            }),
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_or() {
        let cmd = "grep pattern || echo 'not found'";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Or {
            l: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "grep",
                    args: vec!["pattern"],
                },
                _async: false,
            }),
            r: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["not found"],
                },
                _async: false,
            }),
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_pipe() {
        let cmd = "ls | grep .rs";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Pipe {
            l: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "ls",
                    args: vec![],
                },
                _async: false,
            }),
            r: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "grep",
                    args: vec![".rs"],
                },
                _async: false,
            }),
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_redir_output() {
        let cmd = "echo hello > output.txt";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::RedirOutput {
            mode: FileMode::Truncate,
            op: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["hello"],
                },
                _async: false,
            }),
            dest: "output.txt",
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_redir_append() {
        let cmd = "echo hello >> output.txt";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::RedirOutput {
            mode: FileMode::Append,
            op: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["hello"],
                },
                _async: false,
            }),
            dest: "output.txt",
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_redir_input() {
        let cmd = "wc < input.txt";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::RedirInput {
            op: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "wc",
                    args: vec![],
                },
                _async: false,
            }),
            src: "input.txt",
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_subshell() {
        let cmd = "(echo hello)";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Subshell {
            op: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["hello"],
                },
                _async: false,
            }),
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_complex_command() {
        let cmd = "ls -l | grep .rs > output.txt && echo 'done'";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::And {
            l: Box::new(Expr::Pipe {
                l: Box::new(Expr::Cmd {
                    op: Cmd {
                        program: "ls",
                        args: vec!["-l"],
                    },
                    _async: false,
                }),
                r: Box::new(Expr::RedirOutput {
                    mode: FileMode::Truncate,
                    op: Box::new(Expr::Cmd {
                        op: Cmd {
                            program: "grep",
                            args: vec![".rs"],
                        },
                        _async: false,
                    }),
                    dest: "output.txt",
                }),
            }),
            r: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["done"],
                },
                _async: false,
            }),
        };
        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_nested_subshell() {
        let cmd = "echo $(ls $(pwd))";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Cmd {
            op: Cmd {
                program: "echo",
                args: vec!["$(ls $(pwd))"],
            },
            _async: false,
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_multiple_args() {
        let cmd = "git commit -m 'initial commit'";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Cmd {
            op: Cmd {
                program: "git",
                args: vec!["commit", "-m", "initial commit"],
            },
            _async: false,
        };

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    // full program tests

    #[test]
    fn test_simple_program() {
        let cmd = "echo hello\nif [test]; then echo yes; fi";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());

        assert_eq!(program.len(), 2);
        assert!(matches!(program[0], Node::Expression(..)));
        assert!(matches!(program[1], Node::Statement(Stmt::If { .. })));

        if let Node::Expression(Expr::Cmd { op: cmd, .. }) = &program[0] {
            assert_eq!(cmd.program, "echo");
            assert_eq!(cmd.args, vec!["hello"]);
        }

        if let Node::Statement(Stmt::If {
            cond,
            then,
            or_then,
        }) = &program[1]
        {
            assert!(
                matches!(**cond, Expr::Cmd { op: Cmd { program, .. }, ..} if program == "test")
            );
            assert!(
                matches!(**then, Node::Expression(Expr::Cmd{ op: Cmd { program, .. }, ..}) if program == "echo")
            );
            assert!(or_then.is_none());
        }
    }

    #[test]
    fn test_complex_program() {
        let cmd = "
            if [-f file.txt]; then
                cat file.txt | grep pattern > output.txt
            else
                echo 'file not found'
            fi
            while true; do
                echo 'loop'
            done
        ";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());
        assert_eq!(program.len(), 2);

        let expected = Node::Statement(Stmt::If {
            cond: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "test",
                    args: vec!["-f", "file.txt"],
                },
                _async: false,
            }),
            then: Box::new(Node::Expression(Expr::Pipe {
                l: Box::new(Expr::Cmd {
                    op: Cmd {
                        program: "cat",
                        args: vec!["file.txt"],
                    },
                    _async: false,
                }),
                r: Box::new(Expr::RedirOutput {
                    mode: FileMode::Truncate,
                    op: Box::new(Expr::Cmd {
                        op: Cmd {
                            program: "grep",
                            args: vec!["pattern"],
                        },
                        _async: false,
                    }),
                    dest: "output.txt",
                }),
            })),
            or_then: Some(Box::new(Node::Expression(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["file not found"],
                },
                _async: false,
            }))),
        });

        assert_eq!(program[0], expected);

        let expected = Node::Statement(Stmt::While {
            cond: Box::new(Expr::Cmd {
                op: Cmd {
                    program: "true",
                    args: vec![],
                },
                _async: false,
            }),
            body: vec![Node::Expression(Expr::Cmd {
                op: Cmd {
                    program: "echo",
                    args: vec!["loop"],
                },
                _async: false,
            })],
        });

        assert_eq!(program[1], expected);
    }

    #[test]
    fn test_for_loop_program() {
        let cmd = "
            for i in 1 2 3; do
                echo $i
                if test $i = 2; then
                    break
                fi
            done
        ";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());
        assert_eq!(program.len(), 1);

        let expected = Node::Statement(Stmt::For {
            var: "i",
            items: vec!["1", "2", "3"],
            body: vec![
                Node::Expression(Expr::Cmd {
                    op: Cmd {
                        program: "echo",
                        args: vec!["$i"],
                    },
                    _async: false,
                }),
                Node::Statement(Stmt::If {
                    cond: Expr::Cmd {
                        op: Cmd {
                            program: "test",
                            args: vec!["$i", "=", "2"],
                        },
                        _async: false,
                    }
                    .into(),
                    then: Node::Expression(Expr::Cmd {
                        op: Cmd {
                            program: "break",
                            args: vec![],
                        },
                        _async: false,
                    })
                    .into(),
                    or_then: None,
                }),
            ],
        });

        assert_eq!(program[0], expected);
    }

    #[test]
    fn test_nested_blocks() {
        let cmd = "
            {
                echo outer
                {
                    echo inner
                }
                echo after
            }
        ";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());

        assert_eq!(program.len(), 1);

        let expected = Node::Statement(Stmt::Block {
            ops: vec![
                Node::Expression(Expr::Cmd {
                    op: Cmd {
                        program: "echo",
                        args: vec!["outer"],
                    },
                    _async: false,
                }),
                Node::Statement(Stmt::Block {
                    ops: vec![Node::Expression(Expr::Cmd {
                        op: Cmd {
                            program: "echo",
                            args: vec!["inner"],
                        },
                        _async: false,
                    })],
                    _async: false,
                }),
                Node::Expression(Expr::Cmd {
                    op: Cmd {
                        program: "echo",
                        args: vec!["after"],
                    },
                    _async: false,
                }),
            ],
            _async: false,
        });

        assert_eq!(program[0], expected);
    }

    #[test]
    fn test_mixed_expressions_and_statements() {
        let cmd = "
            echo start
            if [true]; then {
                ls -l | grep .txt > files.txt
                for f in *.txt; do
                    cat $f
                done
            } fi
            echo end
        ";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());

        assert_eq!(program.len(), 3);

        let expected0 = Node::Expression(Expr::Cmd {
            op: Cmd {
                program: "echo",
                args: vec!["start"],
            },
            _async: false,
        });

        let expected1 = Node::Statement(Stmt::If {
            cond: Expr::Cmd {
                op: Cmd {
                    program: "test",
                    args: vec!["true"],
                },
                _async: false,
            }
            .into(),
            then: Node::Statement(Stmt::Block {
                ops: vec![
                    Node::Expression(Expr::Pipe {
                        l: Expr::Cmd {
                            op: Cmd {
                                program: "ls",
                                args: vec!["-l"],
                            },
                            _async: false,
                        }
                        .into(),
                        r: Expr::RedirOutput {
                            mode: FileMode::Truncate,
                            op: Expr::Cmd {
                                op: Cmd {
                                    program: "grep",
                                    args: vec![".txt"],
                                },
                                _async: false,
                            }
                            .into(),
                            dest: "files.txt",
                        }
                        .into(),
                    }),
                    Node::Statement(Stmt::For {
                        var: "f",
                        items: vec!["*.txt"],
                        body: vec![Node::Expression(Expr::Cmd {
                            op: Cmd {
                                program: "cat",
                                args: vec!["$f"],
                            },
                            _async: false,
                        })],
                    }),
                ],
                _async: false,
            })
            .into(),
            or_then: None,
        });

        let expected2 = Node::Expression(Expr::Cmd {
            op: Cmd {
                program: "echo",
                args: vec!["end"],
            },
            _async: false,
        });

        assert_eq!(program[0], expected0);
        assert_eq!(program[1], expected1);
        assert_eq!(program[2], expected2);
    }

    #[test]
    fn test_multiple_oneliners() {
        let cmd = "
            echo abc
            echo bca
            echo zxc
        ";

        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());
        assert_eq!(program.len(), 3);
    }

    #[test]
    fn test_error_handling() {
        let cmd = "if true; then echo yes";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());
        assert!(program.is_empty());

        let cmd = "{ echo test";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());
        assert!(program.is_empty());

        // Invalid for loop syntax
        let cmd = "for i in; do echo bad; done";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let program = generate_program(tokens.iter().peekable());
        assert!(program.is_empty());
    }
}
