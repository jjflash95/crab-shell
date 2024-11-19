use std::{fmt::Display, iter::Peekable, slice::Iter};

use crate::lexer::{RedirOp, Token};

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
        condition: Box<Expr<'a>>,
        then: Box<Node<'a>>,
        or_then: Option<Box<Node<'a>>>,
    },
    While {
        condition: Box<Expr<'a>>,
        body: Vec<Node<'a>>,
    },
    For {
        var: &'a str,
        items: Vec<&'a str>,
        body: Vec<Node<'a>>,
    },
    Block(Vec<Node<'a>>),
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
    Subshell(Box<Expr<'a>>),
    Cmd(Cmd<'a>),
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

fn pattern_match_predicate(source: TokenStream) -> Result<(TokenStream, Expr), String> {
    // TODO: parse pattern match
    test_cmd_prefix(source)
}

fn test_cmd_prefix(mut source: TokenStream) -> Result<(TokenStream, Expr), String> {
    source.expect_next(|t| matches!(t, Token::LBracket))?;
    let (mut source, Expr::Cmd(Cmd { program, args })) = cmd(source)? else {
        Err("expected command".to_string())?
    };

    source.expect_next(|t| matches!(t, Token::RBracket))?;
    let mut exp_args = vec![program];
    exp_args.extend(args);

    Ok((
        source,
        Expr::Cmd(Cmd {
            program: "test",
            args: exp_args,
        }),
    ))
}

fn if_node(mut source: TokenStream) -> Result<(TokenStream, Stmt), String> {
    source.expect_next(|t| matches!(t, Token::If))?;

    let (mut source, condition) = if source.peek() == Some(&&Token::LBracket) {
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
            condition: Box::new(condition),
            then: Box::new(then),
            or_then,
        },
    ))
}

fn while_node(mut source: TokenStream) -> Result<(TokenStream, Stmt), String> {
    source.expect_next(|t| matches!(t, Token::While))?;
    let (mut source, condition) = ast(source)?;
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
            condition: Box::new(condition),
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

    Ok((source, Stmt::Block(statements)))
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
    Ok((source, Expr::Cmd(crate::exec::BREAK_PROGRAM)))
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
    Ok((source, Expr::Cmd(cmd)))
}

pub fn grouped(mut source: TokenStream) -> ParseRes {
    let _ = source.expect_next(|t| matches!(t, Token::LBrace))?;
    let (mut source, expr) = ast(source)?;
    let _ = source.expect_next(|t| matches!(t, Token::RBrace))?;
    Ok((source, expr))
}

pub fn subshell(mut source: TokenStream) -> ParseRes {
    let _ = source.expect_next(|t| matches!(t, Token::LParen))?;
    let (mut source, expr) = ast(source)?;
    let _ = source.expect_next(|t| matches!(t, Token::RParen))?;
    Ok((source, Expr::Subshell(expr.into())))
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
    let expr = Expr::Cmd(Cmd { program, args });
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
                condition,
                then,
                or_then,
            } => {
                write!(f, "if {}; then\r\n{}", condition, then)?;
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
            Stmt::Block(body) => {
                write!(f, "{{")?;
                for node in body {
                    write!(f, "\r\n{}", node)?;
                }
                write!(f, "\r\n}}")
            }
            Stmt::While { condition, body } => {
                write!(f, "while {} do;", condition)?;
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
            Expr::Cmd(cmd) => cmd.fmt(f),
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
            Expr::Subshell(ss) => write!(f, "({})", ss),
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

        let expected = Expr::Cmd(Cmd {
            program: "ls",
            args: vec!["-la"],
        });

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_and() {
        let cmd = "echo a && echo b";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::And {
            l: Box::new(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["a"],
            })),
            r: Box::new(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["b"],
            })),
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
            l: Box::new(Expr::Cmd(Cmd {
                program: "grep",
                args: vec!["pattern"],
            })),
            r: Box::new(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["not found"],
            })),
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
            l: Box::new(Expr::Cmd(Cmd {
                program: "ls",
                args: vec![],
            })),
            r: Box::new(Expr::Cmd(Cmd {
                program: "grep",
                args: vec![".rs"],
            })),
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
            op: Box::new(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["hello"],
            })),
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
            op: Box::new(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["hello"],
            })),
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
            op: Box::new(Expr::Cmd(Cmd {
                program: "wc",
                args: vec![],
            })),
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

        let expected = Expr::Subshell(Box::new(Expr::Cmd(Cmd {
            program: "echo",
            args: vec!["hello"],
        })));

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
                l: Box::new(Expr::Cmd(Cmd {
                    program: "ls",
                    args: vec!["-l"],
                })),
                r: Box::new(Expr::RedirOutput {
                    mode: FileMode::Truncate,
                    op: Box::new(Expr::Cmd(Cmd {
                        program: "grep",
                        args: vec![".rs"],
                    })),
                    dest: "output.txt",
                }),
            }),
            r: Box::new(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["done"],
            })),
        };
        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_nested_subshell() {
        let cmd = "echo $(ls $(pwd))";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Cmd(Cmd {
            program: "echo",
            args: vec!["$(ls $(pwd))"],
        });

        assert!(rest.collect::<Vec<_>>().is_empty());
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_multiple_args() {
        let cmd = "git commit -m 'initial commit'";
        let tokens = Tokenizer::new(cmd).collect::<Vec<_>>();
        let (rest, ast) = ast(tokens.iter().peekable()).unwrap();

        let expected = Expr::Cmd(Cmd {
            program: "git",
            args: vec!["commit", "-m", "initial commit"],
        });

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

        if let Node::Expression(Expr::Cmd(cmd)) = &program[0] {
            assert_eq!(cmd.program, "echo");
            assert_eq!(cmd.args, vec!["hello"]);
        }

        if let Node::Statement(Stmt::If {
            condition,
            then,
            or_then,
        }) = &program[1]
        {
            assert!(matches!(**condition, Expr::Cmd(Cmd { program, .. }) if program == "test"));
            assert!(
                matches!(**then, Node::Expression(Expr::Cmd(Cmd { program, .. })) if program == "echo")
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
            condition: Box::new(Expr::Cmd(Cmd {
                program: "test",
                args: vec!["-f", "file.txt"],
            })),
            then: Box::new(Node::Expression(Expr::Pipe {
                l: Box::new(Expr::Cmd(Cmd {
                    program: "cat",
                    args: vec!["file.txt"],
                })),
                r: Box::new(Expr::RedirOutput {
                    mode: FileMode::Truncate,
                    op: Box::new(Expr::Cmd(Cmd {
                        program: "grep",
                        args: vec!["pattern"],
                    })),
                    dest: "output.txt",
                }),
            })),
            or_then: Some(Box::new(Node::Expression(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["file not found"],
            })))),
        });

        assert_eq!(program[0], expected);

        let expected = Node::Statement(Stmt::While {
            condition: Box::new(Expr::Cmd(Cmd {
                program: "true",
                args: vec![],
            })),
            body: vec![Node::Expression(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["loop"],
            }))],
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
                Node::Expression(Expr::Cmd(Cmd {
                    program: "echo",
                    args: vec!["$i"],
                })),
                Node::Statement(Stmt::If {
                    condition: Expr::Cmd(Cmd {
                        program: "test",
                        args: vec!["$i", "=", "2"],
                    })
                    .into(),
                    then: Node::Expression(Expr::Cmd(Cmd {
                        program: "break",
                        args: vec![],
                    }))
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

        let expected = Node::Statement(Stmt::Block(vec![
            Node::Expression(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["outer"],
            })),
            Node::Statement(Stmt::Block(vec![Node::Expression(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["inner"],
            }))])),
            Node::Expression(Expr::Cmd(Cmd {
                program: "echo",
                args: vec!["after"],
            })),
        ]));

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

        let expected0 = Node::Expression(Expr::Cmd(Cmd {
            program: "echo",
            args: vec!["start"],
        }));

        let expected1 = Node::Statement(Stmt::If {
            condition: Expr::Cmd(Cmd {
                program: "test",
                args: vec!["true"],
            })
            .into(),
            then: Node::Statement(Stmt::Block(vec![
                Node::Expression(Expr::Pipe {
                    l: Expr::Cmd(Cmd {
                        program: "ls",
                        args: vec!["-l"],
                    })
                    .into(),
                    r: Expr::RedirOutput {
                        mode: FileMode::Truncate,
                        op: Expr::Cmd(Cmd {
                            program: "grep",
                            args: vec![".txt"],
                        })
                        .into(),
                        dest: "files.txt",
                    }
                    .into(),
                }),
                Node::Statement(Stmt::For {
                    var: "f",
                    items: vec!["*.txt"],
                    body: vec![Node::Expression(Expr::Cmd(Cmd {
                        program: "cat",
                        args: vec!["$f"],
                    }))],
                }),
            ]))
            .into(),
            or_then: None,
        });

        let expected2 = Node::Expression(Expr::Cmd(Cmd {
            program: "echo",
            args: vec!["end"],
        }));

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
    }
}
