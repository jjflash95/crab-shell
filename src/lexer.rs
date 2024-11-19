use std::{iter::Peekable, ops::Deref, str::CharIndices};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Word(&'a str),
    Pipe,
    And,
    Or,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Redirect(RedirOp),
    Background,
    Semicolon,
    QuotedStr(&'a str),
    If,
    Endif,
    Else,
    Then,
    Do,
    Done,
    While,
    For,
    In,
    Newline,
}

impl<'a> From<&'a str> for Token<'a> {
    fn from(value: &'a str) -> Self {
        use Token::*;
        match value {
            "if" => If,
            "else" => Else,
            "then" => Then,
            "do" => Do,
            "done" => Done,
            "while" => While,
            "for" => For,
            "in" => In,
            "fi" => Endif,
            s => Word(s),
        }
    }
}

impl<'a> Token<'a> {
    pub fn into_str(self) -> &'a str {
        match self {
            Self::Word(s) | Self::QuotedStr(s) => s,
            _ => panic!("sorry I fucked up"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RedirOp {
    Input,
    Output,
    Append,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'a> Deref for Tokenizer<'a> {
    type Target = Lexer<'a>;

    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    slice: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            lexer: Lexer::new(s),
        }
    }

    fn take_var(&mut self) -> &'a str {
        let mut iterable = self.as_iter();
        assert!(matches!(iterable.next(), Some((_, '$'))), "sorry I fkd up");

        for (i, c) in iterable {
            if c.is_whitespace() || c == '\n' {
                return self.lexer.take_subslice(i);
            }
        }
        self.lexer.take_subslice(self.lexer.slice.len())
    }

    fn take_subcmd(&mut self) -> Option<&'a str> {
        let mut iterable = self.lexer.as_iter();
        assert!(matches!(iterable.next(), Some((_, '$'))), "sorry I fkd up");
        assert!(matches!(iterable.next(), Some((_, '('))), "sorry I fkd up");

        let mut needed_parens = 0;
        for (i, c) in iterable {
            match c {
                ')' if needed_parens == 0 => {
                    let subcmd = self.lexer.take_subslice(i + 1);
                    return Some(subcmd);
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

    fn next_token(&mut self) -> Option<Token<'a>> {
        use Token::*;

        if self.lexer.is_empty() {
            return None;
        };
        let mut iterable = self.lexer.as_iter();
        match iterable.peek().copied() {
            Some((_, '$')) => {
                let _ = iterable.next();
                if let Some((_, '(')) = iterable.peek() {
                    self.take_subcmd().map(Word)
                } else {
                    Some(Word(self.take_var()))
                }
            }
            Some((i, '|')) => {
                let _ = iterable.next();
                if let Some((i, '|')) = iterable.peek() {
                    let _ = self.lexer.take_subslice(1 + *i);
                    Some(Or)
                } else {
                    let _ = self.lexer.take_subslice(1 + i);
                    Some(Pipe)
                }
            }
            Some((i, '&')) => {
                let _ = iterable.next();
                if let Some((i, '&')) = iterable.peek() {
                    let _ = self.lexer.take_subslice(1 + *i);
                    Some(And)
                } else {
                    let _ = self.lexer.take_subslice(1 + i);
                    Some(Background)
                }
            }
            Some((i, '>')) => {
                let _ = iterable.next();
                if let Some((i, '>')) = iterable.peek() {
                    let _ = self.lexer.take_subslice(1 + *i);
                    Some(Redirect(RedirOp::Append))
                } else {
                    let _ = self.lexer.take_subslice(1 + i);
                    Some(Redirect(RedirOp::Output))
                }
            }
            Some((i, '\\')) => {
                let _ = self.lexer.take_subslice(1 + i);
                self.next_token()
            }
            Some((i, '\n')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(Newline)
            }
            Some((_, '#')) => {
                self.lexer.take_while(|c| c != '\n');
                self.next_token()
            }
            Some((i, '<')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(Redirect(RedirOp::Input))
            }
            Some((i, '{')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(LBrace)
            }
            Some((i, '}')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(RBrace)
            }
            Some((i, '(')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(LParen)
            }
            Some((i, ')')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(RParen)
            }
            Some((i, '[')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(LBracket)
            }
            Some((i, ']')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(RBracket)
            }
            Some((i, ';')) => {
                let _ = self.lexer.take_subslice(1 + i);
                Some(Semicolon)
            }
            Some((_, '"')) | Some((_, '\'')) => self.lexer.quoted_str().map(QuotedStr),
            Some((_, c)) if c.is_whitespace() => {
                let _ = self.lexer.take_while(|c| c.is_whitespace());
                self.next()
            }
            Some(_) => Some(Token::from(self.lexer.take_word())),
            None => None,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self { slice: s }
    }

    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    pub fn as_iter(&self) -> Peekable<CharIndices<'a>> {
        self.slice.char_indices().peekable()
    }

    pub fn take_subslice(&mut self, i: usize) -> &'a str {
        let sub = &self.slice[0..i];
        self.slice = &self.slice[i..];
        sub
    }

    pub fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> &'a str {
        let mut iterable = self.as_iter();
        while let Some((_, c)) = iterable.peek() {
            if !pred(*c) {
                break;
            }
            let _ = iterable.next();
        }

        let range = iterable.peek().map(|(i, _)| *i).unwrap_or(self.slice.len());
        self.take_subslice(range)
    }

    pub fn quoted_str(&mut self) -> Option<&'a str> {
        let mut iterable = self.as_iter();
        let (_, opening_quote) = iterable.next().unwrap();
        for (range, c) in iterable {
            if c == opening_quote {
                let sub = self.take_subslice(range + c.to_string().as_str().len());
                // Remove opening and closing quotes
                return Some(&sub[1..range]);
            }
        }

        None
    }

    pub fn take_word(&mut self) -> &'a str {
        self.take_while(|c| {
            !c.is_whitespace()
                && !matches!(
                    c,
                    '\n' | '|' | '&' | '<' | '>' | '(' | ')' | '{' | '}' | ';' | '[' | ']'
                )
        })
    }
}

#[allow(dead_code)]
pub fn tokenize(src: &str) -> Result<Vec<Token>, &str> {
    let mut lexer = Tokenizer::new(src);
    let mut out = vec![];

    for t in lexer.by_ref() {
        out.push(t);
    }

    if !lexer.is_empty() {
        return Err(lexer.slice);
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_lexer() {
        let cmd = "echo a && echo b";
        let expected = vec![Word("echo"), Word("a"), And, Word("echo"), Word("b")];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_basic_command() {
        let cmd = "echo hello";
        let expected = vec![Token::Word("echo"), Token::Word("hello")];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_logical_operators() {
        let cmd = "echo a && echo b || echo c";
        let expected = vec![
            Token::Word("echo"),
            Token::Word("a"),
            Token::And,
            Token::Word("echo"),
            Token::Word("b"),
            Token::Or,
            Token::Word("echo"),
            Token::Word("c"),
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_pipes_and_redirects() {
        let cmd = "cat file.txt | grep pattern > output.txt";
        let expected = vec![
            Token::Word("cat"),
            Token::Word("file.txt"),
            Token::Pipe,
            Token::Word("grep"),
            Token::Word("pattern"),
            Token::Redirect(RedirOp::Output),
            Token::Word("output.txt"),
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_quotes() {
        let cmd = r#"echo "hello world" 'single quoted'"#;
        let expected = vec![
            Token::Word("echo"),
            Token::QuotedStr("hello world"),
            Token::QuotedStr("single quoted"),
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_subcommands() {
        let cmd = "echo $(cat file.txt)";
        let expected = vec![Token::Word("echo"), Token::Word("$(cat file.txt)")];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_complex_command() {
        let cmd = r#"if [ -f "file.txt" ]; then echo "found" && cat file.txt | grep pattern; fi"#;
        let expected = vec![
            Token::If,
            Token::LBracket,
            Token::Word("-f"),
            Token::QuotedStr("file.txt"),
            Token::RBracket,
            Token::Semicolon,
            Token::Then,
            Token::Word("echo"),
            Token::QuotedStr("found"),
            Token::And,
            Token::Word("cat"),
            Token::Word("file.txt"),
            Token::Pipe,
            Token::Word("grep"),
            Token::Word("pattern"),
            Token::Semicolon,
            Token::Endif,
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_empty_input() {
        let cmd = "";
        let expected: Vec<Token> = vec![];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_whitespace_handling() {
        let cmd = "  echo   hello  \t  world  \n";
        let expected = vec![
            Token::Word("echo"),
            Token::Word("hello"),
            Token::Word("world"),
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_error_cases() {
        assert!(tokenize(r#"echo "unterminated"#).is_err());
        assert!(tokenize(r#"echo 'unterminated single quote"#).is_err());
    }

    #[test]
    fn test_background_jobs() {
        let cmd = "long_task & echo 'done'";
        let expected = vec![
            Token::Word("long_task"),
            Token::Background,
            Token::Word("echo"),
            Token::QuotedStr("done"),
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }

    #[test]
    fn test_var_newline() {
        use Token::*;

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

        let expected = vec![
            Newline,
            Word("echo"),
            Word("start"),
            Newline,
            If,
            LBracket,
            Word("true"),
            RBracket,
            Semicolon,
            Then,
            LBrace,
            Newline,
            Word("ls"),
            Word("-l"),
            Pipe,
            Word("grep"),
            Word(".txt"),
            Redirect(RedirOp::Output),
            Word("files.txt"),
            Newline,
            For,
            Word("f"),
            In,
            Word("*.txt"),
            Semicolon,
            Do,
            Newline,
            Word("cat"),
            Word("$f"),
            Newline,
            Done,
            Newline,
            RBrace,
            Endif,
            Newline,
            Word("echo"),
            Word("end"),
            Newline,
        ];
        assert_eq!(tokenize(cmd), Ok(expected));
    }
}
