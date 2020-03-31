#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    LParen,
    RParen,
    True,
    False,
    Not,
    If,
    Then,
    Else,
    Let,
    Rec,
    In,
    Number(u64),
    Minus,
    MinusDot,
    Plus,
    PlusDot,
    AstDot,
    SlashDot,
    Equal,
    LessGreater,
    LessEqual,
    LessMinus,
    Less,
    GreaterEqual,
    Greater,
    Dot,
    Comma,
    Semicolon,
    ArrayCreate,
    Id(String),
}

#[derive(Debug)]
pub enum LexErr {
    EndOfInput,
    UnterminatedComment,
    UnexpectedChar { expected: u8, found: u8 },
    UnexpectedUppercaseChar { found: u8 },
}

pub struct Lexer<'a> {
    input: &'a [u8],
    // Current position in `input`
    byte_idx: usize,
    id_buf: String,
}

static ARRAY_CREATE_STR: &str = "Array.create";
static ARRAY_CREATE_LEN: usize = ARRAY_CREATE_STR.len();

static ARRAY_MAKE_STR: &str = "Array.make";
static ARRAY_MAKE_LEN: usize = ARRAY_MAKE_STR.len();

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Lexer {
        Lexer {
            input,
            byte_idx: 0,
            id_buf: String::with_capacity(20),
        }
    }

    pub fn next(&mut self) -> Result<Token, LexErr> {
        loop {
            match self.next_byte()? {
                next if next.is_ascii_whitespace() => {
                    self.consume();
                }
                b'(' => {
                    self.consume();
                    if let Ok(b'*') = self.next_byte() {
                        self.consume();
                        self.consume_comment()?;
                    } else {
                        return Ok(Token::LParen);
                    }
                }
                b')' => {
                    self.consume();
                    return Ok(Token::RParen);
                }
                b'-' => {
                    self.consume();
                    if let Ok(b'.') = self.next_byte() {
                        self.consume();
                        return Ok(Token::MinusDot);
                    } else {
                        return Ok(Token::Minus);
                    }
                }
                b'+' => {
                    self.consume();
                    if let Ok(b'.') = self.next_byte() {
                        self.consume();
                        return Ok(Token::PlusDot);
                    } else {
                        return Ok(Token::Plus);
                    }
                }
                b'*' => {
                    self.consume();
                    self.expect_char(b'.')?;
                    return Ok(Token::AstDot);
                }
                b'/' => {
                    self.consume();
                    self.expect_char(b'.')?;
                    return Ok(Token::SlashDot);
                }
                b'=' => {
                    self.consume();
                    return Ok(Token::Equal);
                }
                b'<' => {
                    self.consume();
                    let next = self.next_byte();
                    match next {
                        Ok(b'>') => {
                            self.consume();
                            return Ok(Token::LessGreater);
                        }
                        Ok(b'=') => {
                            self.consume();
                            return Ok(Token::LessEqual);
                        }
                        Ok(b'-') => {
                            self.consume();
                            return Ok(Token::LessMinus);
                        }
                        _ => {
                            return Ok(Token::Less);
                        }
                    }
                }
                b'>' => {
                    self.consume();
                    let next = self.next_byte();
                    match next {
                        Ok(b'=') => {
                            self.consume();
                            return Ok(Token::GreaterEqual);
                        }
                        _ => {
                            return Ok(Token::Greater);
                        }
                    }
                }
                b'.' => {
                    self.consume();
                    return Ok(Token::Dot);
                }
                b',' => {
                    self.consume();
                    return Ok(Token::Comma);
                }
                b';' => {
                    self.consume();
                    return Ok(Token::Semicolon);
                }
                _ => {
                    return self.expect_kw_or_id();
                }
            }
        }
    }

    fn expect_kw_or_id(&mut self) -> Result<Token, LexErr> {
        let next = self.next_byte()?;
        if next.is_ascii_lowercase() {
            self.consume();
            self.id_buf.push(char::from(next));
            loop {
                let next = self.next_byte();
                match next {
                    Err(_) => {
                        return Ok(self.process_id());
                    }
                    Ok(next) => {
                        if next.is_ascii_digit()
                            || next.is_ascii_lowercase()
                            || next.is_ascii_uppercase()
                            || next == b'_'
                        {
                            self.consume();
                            self.id_buf.push(char::from(next));
                        } else {
                            return Ok(self.process_id());
                        }
                    }
                }
            }
        } else {
            // Array.create or Array.make
            let rest = &self.input[self.byte_idx..];
            if rest.len() < ARRAY_CREATE_LEN
                && &rest[0..ARRAY_CREATE_LEN] == ARRAY_CREATE_STR.as_bytes()
            {
                Ok(Token::ArrayCreate)
            } else if rest.len() < ARRAY_MAKE_LEN
                && &rest[0..ARRAY_MAKE_LEN] == ARRAY_MAKE_STR.as_bytes()
            {
                Ok(Token::ArrayCreate)
            } else {
                Err(LexErr::UnexpectedUppercaseChar { found: next })
            }
        }
    }

    fn process_id(&mut self) -> Token {
        debug_assert!(!self.id_buf.is_empty());
        let id = ::std::mem::replace(&mut self.id_buf, String::with_capacity(20));
        match id.as_str() {
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "let" => Token::Let,
            "in" => Token::In,
            "rec" => Token::Rec,
            "true" => Token::True,
            "false" => Token::False,
            "not" => Token::Not,
            _ => Token::Id(id),
        }
    }

    fn expect_char(&mut self, char: u8) -> Result<(), LexErr> {
        let next = self.next_byte()?;
        if next == char {
            Ok(())
        } else {
            Err(LexErr::UnexpectedChar {
                expected: char,
                found: next,
            })
        }
    }

    fn consume_comment(&mut self) -> Result<(), LexErr> {
        let mut level = 1;
        while level != 0 {
            match self.next_byte()? {
                b'(' => {
                    self.consume();
                    if let Ok(b'*') = self.next_byte() {
                        self.consume();
                        level += 1;
                    }
                }
                b'*' => {
                    self.consume();
                    if let Ok(b')') = self.next_byte() {
                        self.consume();
                        level -= 1;
                    }
                }
                _ => {
                    self.consume();
                }
            }
        }
        Ok(())
    }

    fn next_byte(&self) -> Result<u8, LexErr> {
        if self.byte_idx == self.input.len() {
            Err(LexErr::EndOfInput)
        } else {
            Ok(self.input[self.byte_idx])
        }
    }

    fn consume(&mut self) {
        self.byte_idx += 1;
    }
}

#[test]
fn lexer_test() {
    let input = "(* blah blah *) > < <> = +. - +";
    let mut lexer = Lexer::new(input.as_bytes());
    assert_eq!(lexer.next().unwrap(), Token::Greater);
    assert_eq!(lexer.next().unwrap(), Token::Less);
    assert_eq!(lexer.next().unwrap(), Token::LessGreater);
    assert_eq!(lexer.next().unwrap(), Token::Equal);
    assert_eq!(lexer.next().unwrap(), Token::PlusDot);
    assert_eq!(lexer.next().unwrap(), Token::Minus);
    assert_eq!(lexer.next().unwrap(), Token::Plus);
    match lexer.next() {
        Err(LexErr::EndOfInput) => {}
        other => {
            panic!("{:#?}", other);
        }
    }
}
