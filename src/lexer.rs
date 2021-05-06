use std::str::FromStr;

use lexgen::lexer;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LParen,
    RParen,
    Bool(bool),
    Not,
    If,
    Then,
    Else,
    Let,
    Rec,
    In,
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
    Underscore,
    ArrayCreate,
    Id(String),
    Int(i64),
    Float(f64),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexErr {
    InvalidFloat { found: String },
    InvalidInt { found: String },
}

#[derive(Debug, Default)]
pub struct LexerState {
    comment_depth: usize,
}

pub fn tokenize(expr_str: &str) -> Result<Vec<Token>, LexerError> {
    let lexer = Lexer::new(expr_str);
    let mut tokens = vec![];
    for next in lexer {
        match next {
            Ok(token) => tokens.push(token.1),
            Err(err) => return Err(err),
        }
    }
    Ok(tokens)
}

lexer! {
    pub Lexer(LexerState) -> Token;

    type Error = LexErr;

    rule Init {
        [' ' '\t' '\n']+,

        "(" = Token::LParen,
        ")" = Token::RParen,
        "true" = Token::Bool(true),
        "false" = Token::Bool(false),
        "not" = Token::Not,
        "if" = Token::If,
        "then" = Token::Then,
        "else" = Token::Else,
        "let" = Token::Let,
        "rec" = Token::Rec,
        "in" = Token::In,
        "-" = Token::Minus,
        "-." = Token::MinusDot,
        "+" = Token::Plus,
        "+." = Token::PlusDot,
        "*." = Token::AstDot,
        "/." = Token::SlashDot,
        "=" = Token::Equal,
        "<>" = Token::LessGreater,
        "<=" = Token::LessEqual,
        "<-" = Token::LessMinus,
        "<" = Token::Less,
        ">=" = Token::GreaterEqual,
        ">" = Token::Greater,
        "." = Token::Dot,
        "," = Token::Comma,
        ";" = Token::Semicolon,
        "_" = Token::Underscore,
        "Array.create" = Token::ArrayCreate,
        "Array.make" = Token::ArrayCreate,

        ['a'-'z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* =>
            |lexer| {
                let match_ = lexer.match_();
                lexer.return_(Token::Id(match_.to_owned()))
            },

        ['0'-'9']+ =?
            |lexer| {
                let match_ = lexer.match_();
                match i64::from_str(match_) {
                    Ok(i) => lexer.return_(Ok(Token::Int(i))),
                    Err(_) => lexer.return_(Err(LexErr::InvalidInt { found: match_.to_owned() })),
                }
            },

        ['0'-'9']+ '.' ['0'-'9']* =?
            |lexer| {
                let match_ = lexer.match_();
                match f64::from_str(match_) {
                    Ok(f) => lexer.return_(Ok(Token::Float(f))),
                    Err(_) => lexer.return_(Err(LexErr::InvalidFloat { found: match_.to_owned() })),
                }
            },

        "(*" =>
            |mut lexer| {
                lexer.state().comment_depth = 1;
                lexer.switch(LexerRule::Comment)
            },
    },

    rule Comment {
        "(*" =>
            |mut lexer| {
                let depth = &mut lexer.state().comment_depth;
                *depth =  *depth + 1;
                lexer.continue_()
            },

        "*)" =>
            |mut lexer| {
                let depth = &mut lexer.state().comment_depth;
                if *depth == 1 {
                    lexer.switch(LexerRule::Init)
                } else {
                    *depth = *depth - 1;
                    lexer.continue_()
                }
            },

        _,
    },
}

#[cfg(test)]
fn unwrap_ignore_pos<T, E: std::fmt::Debug>(token: Option<Result<(usize, T, usize), E>>) -> T {
    token.unwrap().unwrap().1
}

#[test]
fn lexer_test() {
    let input = "(* test *) (* (* (* ** *) *) *) > < <> = +. - + let rec";
    let mut lexer = Lexer::new(input);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Greater);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Less);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::LessGreater);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Equal);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::PlusDot);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Minus);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Plus);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Let);
    assert_eq!(unwrap_ignore_pos(lexer.next()), Token::Rec);
    assert_eq!(lexer.next(), None);
}
