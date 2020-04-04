use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    // ()
    Unit,
    // true, false
    Bool(bool),
    Int(u64),
    Float(f64),
    // not <expr>
    Not(Box<Expr>),
    // - <expr>
    Neg(Box<Expr>),
    // <expr> + <expr>
    Add(Box<Expr>, Box<Expr>),
    // <expr> - <expr>
    Sub(Box<Expr>, Box<Expr>),
    // -. <expr>
    FNeg(Box<Expr>),
    // <expr> +. <expr>
    FAdd(Box<Expr>, Box<Expr>),
    // <expr> -. <expr>
    FSub(Box<Expr>, Box<Expr>),
    // <expr> *. <expr>
    FMul(Box<Expr>, Box<Expr>),
    // <expr> /. <expr>
    FDiv(Box<Expr>, Box<Expr>),
    // <expr> = <expr>
    Eq(Box<Expr>, Box<Expr>),
    // <expr> <= <expr>
    Le(Box<Expr>, Box<Expr>),
    // if <expr> then <expr> else <expr>
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    // let <ident> = <expr> in <expr>
    Let {
        id: String,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    // <ident>
    Var(String),
    // let rec <ident> <ident>+ = <expr> in <expr>
    LetRec {
        name: String,
        args: Vec<String>,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    // <expr> <expr>+
    App {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    // <expr> (, <expr>)+
    Tuple(Vec<Expr>),
    // let ( <ident> (, <ident>)+ ) = <expr> in <expr>
    LetTuple {
        bndrs: Vec<String>,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    // Array.create <expr> <expr>
    Array(Box<Expr>, Box<Expr>),
    // <expr> . ( <expr> )
    Get(Box<Expr>, Box<Expr>),
    // <expr> . ( <expr> ) <- <expr>
    Put(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum ParseErr {
    EndOfInput,
    Unexpected { seen: Token, expected: &'static str },
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    tok_idx: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser { tokens, tok_idx: 0 }
    }

    pub fn expr0(&mut self) -> Result<Expr, ParseErr> {
        match self.next_token()? {
            //
            // Single-token expressions
            //
            Token::Bool(bool) => {
                let b = *bool;
                self.consume();
                Ok(Expr::Bool(b))
            }
            Token::Int(int) => {
                let i = *int;
                self.consume();
                Ok(Expr::Int(i))
            }
            Token::Float(float) => {
                let f = *float;
                self.consume();
                Ok(Expr::Float(f))
            }
            Token::Id(id) => {
                let id = id.to_owned();
                self.consume();
                Ok(Expr::Var(id))
            }

            //
            // Other stuff
            //
            Token::LParen => {
                self.consume();
                match self.next_token()? {
                    Token::RParen => {
                        self.consume();
                        Ok(Expr::Unit)
                    }
                    _ => {
                        let expr = self.expr1()?;
                        self.expect(Token::RParen)?;
                        Ok(expr)
                    }
                }
            }
            Token::Not => {
                self.consume();
                Ok(Expr::Not(Box::new(self.expr1()?)))
            }
            Token::Minus => {
                self.consume();
                if let Ok(Token::Dot) = self.next_token() {
                    self.consume();
                    Ok(Expr::FNeg(Box::new(self.expr1()?)))
                } else {
                    Ok(Expr::Neg(Box::new(self.expr1()?)))
                }
            }
            Token::ArrayCreate => {
                self.consume();
                let expr1 = self.expr1()?;
                let expr2 = self.expr1()?;
                Ok(Expr::Array(Box::new(expr1), Box::new(expr2)))
            }
            Token::Let => {
                self.consume();
                match self.next_token()? {
                    Token::Rec => todo!(),
                    Token::LParen => todo!(),
                    Token::Id(var) => {
                        let id = var.clone();
                        self.consume();
                        self.expect(Token::Equal)?;
                        let rhs = Box::new(self.expr1()?);
                        self.expect(Token::In)?;
                        let body = Box::new(self.expr1()?);
                        Ok(Expr::Let { id, rhs, body })
                    }
                    other => {
                        Err(ParseErr::Unexpected {
                            // TODO: remove cloning
                            seen: other.clone(),
                            expected: "'rec', '(', or identifier",
                        })
                    }
                }
            }
            Token::If => {
                self.consume();
                let e1 = self.expr1()?;
                self.expect(Token::Then)?;
                let e2 = self.expr1()?;
                self.expect(Token::Else)?;
                let e3 = self.expr1()?;
                Ok(Expr::If(Box::new(e1), Box::new(e2), Box::new(e3)))
            }
            other => Err(ParseErr::Unexpected {
                seen: other.clone(),
                expected: "",
            }),
        }
    }

    pub fn expr1(&mut self) -> Result<Expr, ParseErr> {
        let expr = self.expr0()?;
        match self.next_token() {
            Ok(Token::Plus) => {
                self.consume();
                let expr2 = self.expr1()?;
                Ok(Expr::Add(Box::new(expr), Box::new(expr2)))
            }
            Ok(Token::Minus) => {
                self.consume();
                let expr2 = self.expr1()?;
                Ok(Expr::Sub(Box::new(expr), Box::new(expr2)))
            }
            _ => Ok(expr),
        }
    }

    fn ident(&mut self) -> Result<String, ParseErr> {
        match self.next_token()? {
            Token::Id(str) => Ok(str.clone()),
            other => Err(ParseErr::Unexpected {
                seen: other.clone(),
                expected: "identifier",
            }),
        }
    }

    fn expect(&mut self, tok: Token) -> Result<(), ParseErr> {
        if *self.next_token()? == tok {
            self.consume();
            Ok(())
        } else {
            todo!()
        }
    }

    fn consume(&mut self) {
        self.tok_idx += 1;
    }

    fn next_token(&self) -> Result<&Token, ParseErr> {
        match self.tokens.get(self.tok_idx) {
            None => Err(ParseErr::EndOfInput),
            Some(next) => Ok(next),
        }
    }
}
