use std::{iter::Peekable, slice::Iter};

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
pub enum Stmt {
    If {},
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
            Some(t) => Err(format!("Unexpected token {t:?}")),
            None => Err("Unexpected EOT".to_string()),
        }
    }
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
    try_parsers!(source, set, grouped, subshell, cmd)
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
        program: "setvar",
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

pub fn generate_program<'a>(mut tokens: Peekable<Iter<'a, Token<'a>>>) -> Vec<Expr<'a>> {
    let mut program = vec![];

    while let Ok((rest, expr)) = ast(tokens) {
        program.push(expr);
        tokens = rest;
    }

    program
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

    #[test]
    fn test_complex_parse() {
        
    }
}
