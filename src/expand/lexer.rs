use crate::lang::lexer::Lexer;

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
    CmdExpansion(&'a str),
    VariableExpr(&'a str),
    Variable(&'a str),
    Whitespace(&'a str),
    Quoted(&'a str),
    Word(&'a str),
}

#[derive(Debug)]
pub struct ArgTokenizer<'a> {
    lexer: Lexer<'a>,
}

impl<'a> ArgTokenizer<'a> {
    pub fn new(slice: &'a str) -> Self {
        Self {
            lexer: Lexer::new(slice),
        }
    }

    fn take_no_pound_word(&mut self) -> &'a str {
        self.lexer.take_while(|c| !c.is_whitespace() && c != '$')
    }

    fn take_variable(&mut self) -> &'a str {
        self.lexer
            .take_while(|c| !c.is_whitespace() && c != '$' && c != '/' && c != ':')
    }

    fn take_cmd_expansion(&mut self) -> Option<Token<'a>> {
        let mut needed_parens = 0;
        let iterable = self.lexer.as_iter();

        for (i, c) in iterable {
            match c {
                ')' if needed_parens == 0 => {
                    let subcmd = self.lexer.take_subslice(i);
                    let _ = self.lexer.take_subslice(1);
                    return Some(Token::CmdExpansion(subcmd));
                }
                ')' => {
                    needed_parens -= 1;
                }
                '(' => {
                    needed_parens += 1;
                }
                _ => {}
            }
        }
        None
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        use Token::*;

        if self.lexer.is_empty() {
            return None;
        }

        let mut iterable = self.lexer.as_iter();

        match iterable.peek().copied() {
            Some((_, c)) if c.is_whitespace() => {
                let _ = iterable.next();
                let whitespace = self.lexer.take_while(|c| c.is_whitespace());
                Some(Whitespace(whitespace))
            }
            Some((_, '$')) => {
                let _ = iterable.next();
                if let Some((i, '{')) = iterable.peek() {
                    let _ = self.lexer.take_subslice(i + 1);
                    let expr = self.lexer.take_while(|c| c != '}');
                    // remove trailing '}'
                    if self.lexer.is_empty() {
                        // unclosed  '{'
                        return None;
                    }
                    let _ = self.lexer.take_subslice(1);
                    return Some(VariableExpr(expr));
                }
                if let Some((i, '(')) = iterable.peek() {
                    let _ = self.lexer.take_subslice(i + 1);
                    return self.take_cmd_expansion();
                }
                let _ = self.lexer.take_subslice(1);
                let variable = self.take_variable();
                Some(Variable(variable))
            }
            Some((i, '"')) | Some((i, '\'')) => {
                let (_, marker) = iterable.next().unwrap();
                let _ = self.lexer.take_subslice(i + 1);
                let string = self.lexer.take_while(|c| c != marker);
                let _ = self.lexer.take_subslice(1);
                Some(Quoted(string))
            }
            Some(_) => Some(Word(self.take_no_pound_word())),
            None => None,
        }
    }
}

impl<'a> Iterator for ArgTokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub fn tokenize(slice: &str) -> Vec<Token> {
    ArgTokenizer::new(slice).collect()
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn test_expansion() {
        let text = "echo $user";
        let expected = vec![Word("echo"), Whitespace(" "), Variable("user")];

        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_basic_command() {
        let text = "ls -l";
        let expected = vec![Token::Word("ls"), Token::Whitespace(" "), Token::Word("-l")];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_multiple_variables() {
        let text = "echo $HOME/$USER";
        let expected = vec![
            Token::Word("echo"),
            Token::Whitespace(" "),
            Token::Variable("HOME"),
            Token::Word("/"),
            Token::Variable("USER"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_multiple_variables_quoted() {
        let text = "echo \"$HOME/$USER\"";
        let expected = vec![
            Token::Word("echo"),
            Token::Whitespace(" "),
            Token::Quoted("$HOME/$USER"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_command_expansion() {
        let text = "echo $(pwd)";
        let expected = vec![
            Token::Word("echo"),
            Token::Whitespace(" "),
            Token::CmdExpansion("pwd"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_multiple_whitespace() {
        let text = "echo    $HOME";
        let expected = vec![
            Token::Word("echo"),
            Token::Whitespace("    "),
            Token::Variable("HOME"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_complex_command() {
        let text = "cp $SRC/$(basename $FILE) $DEST/";
        let expected = vec![
            Token::Word("cp"),
            Token::Whitespace(" "),
            Token::Variable("SRC"),
            Token::Word("/"),
            Token::CmdExpansion("basename $FILE"),
            Token::Whitespace(" "),
            Token::Variable("DEST"),
            Token::Word("/"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_empty_input() {
        let text = "";
        let expected = vec![];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_only_whitespace() {
        let text = "   ";
        let expected = vec![Token::Whitespace("   ")];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_nested_expressions() {
        let text = "echo $(echo $(pwd))";
        let expected = vec![
            Token::Word("echo"),
            Token::Whitespace(" "),
            Token::CmdExpansion("echo $(pwd)"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_variable_with_braces() {
        let text = "echo ${HOME}";
        let expected = vec![
            Token::Word("echo"),
            Token::Whitespace(" "),
            Token::VariableExpr("HOME"),
        ];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_consecutive_variables() {
        let text = "$VAR1$VAR2";
        let expected = vec![Token::Variable("VAR1"), Token::Variable("VAR2")];
        let tokens = ArgTokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }
}
