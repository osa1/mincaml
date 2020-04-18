use crate::lexer::Token;

#[derive(Debug, PartialEq)]
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

pub fn parse(tokens: &[Token]) -> Result<Expr, ParseErr> {
    Parser::new(tokens).expr()
}

#[derive(Debug)]
pub enum ParseErr {
    EndOfInput,
    Unexpected { seen: Token, expected: &'static str },
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    tok_idx: usize,
    gensym_count: usize,
}

const INIT_PREC: usize = 0;
const IN_PREC: usize = 0;
const LET_PREC: usize = 1;
const SEMICOLON_PREC: usize = 2;
const IF_PREC: usize = 3;
// The arrow in `x.(y) <- blah`
const LESS_MINUS_PREC: usize = 4;
const TUPLE_PREC: usize = 5;
const COMMA_PREC: usize = 6;
// Comparison operators
const CMP_PREC: usize = 7;
// Plus and minus, for floats and ints
const PLUS_MINUS_PREC: usize = 8;
// Multiplication and division, for floats and ints
const DIV_MULT_PREC: usize = 9;
const UNARY_MINUS_PREC: usize = 10;
// Function application, `not`, and `Array.create`
const APP_PREC: usize = 11;
// Dots in `x.(y)` (both for getting and setting)
const DOT_PREC: usize = 12;

impl<'a> Parser<'a> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser {
            tokens,
            tok_idx: 0,
            gensym_count: 0,
        }
    }

    pub fn expr0(&mut self, prec: usize) -> Result<Expr, ParseErr> {
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
                        let expr = self.expr1(INIT_PREC)?;
                        self.expect(Token::RParen, "')'")?;
                        Ok(expr)
                    }
                }
            }
            Token::Not if prec <= APP_PREC => {
                self.consume();
                Ok(Expr::Not(Box::new(self.expr1(APP_PREC)?)))
            }
            Token::Minus if prec <= UNARY_MINUS_PREC => {
                self.consume();
                Ok(Expr::Neg(Box::new(self.expr1(UNARY_MINUS_PREC)?)))
            }
            Token::MinusDot if prec <= UNARY_MINUS_PREC => {
                self.consume();
                Ok(Expr::FNeg(Box::new(self.expr1(PLUS_MINUS_PREC)?)))
            }
            Token::ArrayCreate if prec <= APP_PREC => {
                self.consume();
                let expr1 = self.expr0(APP_PREC)?;
                let expr2 = self.expr0(APP_PREC)?;
                Ok(Expr::Array(Box::new(expr1), Box::new(expr2)))
            }
            Token::Let if prec <= LET_PREC => {
                self.consume();
                match self.next_token()? {
                    Token::Rec => {
                        self.consume();
                        let name = self.expect_id()?;
                        let mut args = vec![];
                        while let Ok(Token::Id(arg)) = self.next_token() {
                            args.push(arg.clone());
                            self.consume();
                        }
                        self.expect(Token::Equal, "'='")?;
                        let rhs = Box::new(self.expr1(LET_PREC)?);
                        self.expect(Token::In, "'in'")?;
                        let body = Box::new(self.expr1(LET_PREC)?);
                        Ok(Expr::LetRec {
                            name,
                            args,
                            rhs,
                            body,
                        })
                    }
                    Token::LParen => {
                        self.consume();
                        let mut bndrs = vec![];
                        bndrs.push(self.expect_id()?);
                        while let Ok(Token::Comma) = self.next_token() {
                            self.consume();
                            bndrs.push(self.expect_id()?);
                        }
                        self.expect(Token::RParen, "')'")?;
                        self.expect(Token::Equal, "'='")?;
                        let rhs = self.expr1(LET_PREC)?;
                        self.expect(Token::In, "in")?;
                        let body = self.expr1(INIT_PREC)?;
                        Ok(Expr::LetTuple {
                            bndrs,
                            rhs: Box::new(rhs),
                            body: Box::new(body),
                        })
                    }
                    Token::Id(var) => {
                        let id = var.clone();
                        self.consume();
                        self.expect(Token::Equal, "'='")?;
                        let rhs = Box::new(self.expr1(LET_PREC)?);
                        self.expect(Token::In, "'in'")?;
                        let body = Box::new(self.expr1(IN_PREC)?);
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
            Token::If if prec <= IF_PREC => {
                self.consume();
                let e1 = self.expr1(IF_PREC)?;
                self.expect(Token::Then, "'then'")?;
                let e2 = self.expr1(IF_PREC)?;
                self.expect(Token::Else, "'else'")?;
                let e3 = self.expr1(IF_PREC)?;
                Ok(Expr::If(Box::new(e1), Box::new(e2), Box::new(e3)))
            }
            other => Err(ParseErr::Unexpected {
                seen: other.clone(),
                expected: "expression",
            }),
        }
    }

    pub fn expr1(&mut self, prec: usize) -> Result<Expr, ParseErr> {
        let mut expr = self.expr0(prec)?;
        let mut parsing_app = false;
        loop {
            match self.next_token() {
                Ok(Token::Semicolon) if prec <= SEMICOLON_PREC => {
                    self.consume();
                    let sym = self.gensym();
                    let expr2 = self.expr1(prec)?;
                    expr = Expr::Let {
                        id: sym,
                        rhs: Box::new(expr),
                        body: Box::new(expr2),
                    };
                }
                Ok(Token::Plus) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(PLUS_MINUS_PREC)?;
                    expr = Expr::Add(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::Minus) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(PLUS_MINUS_PREC)?;
                    expr = Expr::Sub(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::PlusDot) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(PLUS_MINUS_PREC)?;
                    expr = Expr::FAdd(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::MinusDot) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(PLUS_MINUS_PREC)?;
                    expr = Expr::FSub(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::AstDot) if prec < DIV_MULT_PREC => {
                    self.consume();
                    let expr2 = self.expr1(DIV_MULT_PREC)?;
                    expr = Expr::FMul(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::SlashDot) if prec < DIV_MULT_PREC => {
                    self.consume();
                    let expr2 = self.expr1(DIV_MULT_PREC)?;
                    expr = Expr::FDiv(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::LessGreater) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(CMP_PREC)?;
                    expr = Expr::Not(Box::new(Expr::Eq(Box::new(expr), Box::new(expr2))));
                }
                Ok(Token::LessEqual) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(CMP_PREC)?;
                    expr = Expr::Le(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::Less) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(CMP_PREC)?;
                    expr = Expr::Le(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::Equal) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(CMP_PREC)?;
                    expr = Expr::Eq(Box::new(expr), Box::new(expr2));
                }
                Ok(Token::Greater) if prec <= CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(CMP_PREC)?;
                    expr = Expr::Not(Box::new(Expr::Le(Box::new(expr), Box::new(expr2))));
                }
                Ok(Token::Comma) if prec <= TUPLE_PREC => {
                    self.consume();
                    let expr2 = self.expr1(COMMA_PREC)?;
                    match expr {
                        Expr::Tuple(ref mut vec) => {
                            vec.push(expr2);
                        }
                        _ => {
                            expr = Expr::Tuple(vec![expr, expr2]);
                        }
                    }
                }
                Ok(Token::Dot) if prec < DOT_PREC => {
                    self.consume();
                    self.expect(Token::LParen, "'('")?;
                    let expr1 = self.expr1(INIT_PREC)?;
                    self.expect(Token::RParen, "')'")?;
                    match self.next_token() {
                        Ok(Token::LessMinus) => {
                            self.consume();
                            let expr2 = self.expr1(LESS_MINUS_PREC)?;
                            match expr {
                                Expr::App { mut args, fun } if parsing_app => {
                                    let arg = args.pop().unwrap();
                                    expr = Expr::App {
                                        fun,
                                        args: vec![Expr::Put(
                                            Box::new(arg),
                                            Box::new(expr1),
                                            Box::new(expr2),
                                        )],
                                    };
                                }
                                _ => {
                                    expr =
                                        Expr::Put(Box::new(expr), Box::new(expr1), Box::new(expr2));
                                }
                            }
                        }
                        _ => match expr {
                            Expr::App { mut args, fun } if parsing_app => {
                                let arg = args.pop().unwrap();
                                expr = Expr::App {
                                    fun,
                                    args: vec![Expr::Get(Box::new(arg), Box::new(expr1))],
                                };
                            }
                            _ => {
                                expr = Expr::Get(Box::new(expr), Box::new(expr1));
                            }
                        },
                    }
                }
                Ok(_) if prec <= APP_PREC => match self.expr0(APP_PREC) {
                    Err(_) => {
                        break;
                    }
                    Ok(expr_) => match expr {
                        Expr::App { ref mut args, .. } if parsing_app => {
                            args.push(expr_);
                        }
                        _ => {
                            parsing_app = true;
                            expr = Expr::App {
                                fun: Box::new(expr),
                                args: vec![expr_],
                            };
                        }
                    },
                },
                _ => {
                    break;
                }
            }
        }

        Ok(expr)
    }

    // Entry point for parsing
    pub fn expr(&mut self) -> Result<Expr, ParseErr> {
        let ret = self.expr1(INIT_PREC)?;
        match self.next_token() {
            Err(_) => Ok(ret),
            Ok(next) => Err(ParseErr::Unexpected {
                seen: next.clone(),
                expected: "EOF",
            }),
        }
    }

    fn gensym(&mut self) -> String {
        let i = self.gensym_count;
        self.gensym_count += 1;
        format!("__{}", i)
    }

    fn expect(&mut self, tok: Token, str: &'static str) -> Result<(), ParseErr> {
        let next_token = self.next_token()?;
        if next_token == &tok {
            self.consume();
            Ok(())
        } else {
            Err(ParseErr::Unexpected {
                seen: next_token.clone(),
                expected: str,
            })
        }
    }

    fn expect_id(&mut self) -> Result<String, ParseErr> {
        match self.next_token()? {
            Token::Id(id) => {
                let id = id.clone();
                self.consume();
                Ok(id)
            }
            other => Err(ParseErr::Unexpected {
                seen: other.clone(),
                expected: "identifier",
            }),
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

/*
Not sure about whether this should parse of not...
#[test]
fn app_parsing() {
    let code = "f Array.create 1 2";
    let tokens = crate::lexer::tokenize(code).unwrap();
    let expr = parse(&tokens).unwrap();
    // eprintln!("{:#?}", expr);
}
*/

#[test]
fn app_parsing_1() {
    let code = "f 1 2";
    let tokens = crate::lexer::tokenize(code).unwrap();
    let expr = parse(&tokens).unwrap();
    assert_eq!(
        expr,
        Expr::App {
            fun: Box::new(Expr::Var("f".to_string())),
            args: vec![Expr::Int(1), Expr::Int(2)]
        }
    );
}

#[test]
fn app_parsing_2() {
    let code = "(f 1) 2";
    let tokens = crate::lexer::tokenize(code).unwrap();
    let expr = parse(&tokens).unwrap();
    assert_eq!(
        expr,
        Expr::App {
            fun: Box::new(Expr::App {
                fun: Box::new(Expr::Var("f".to_string())),
                args: vec![Expr::Int(1)]
            }),
            args: vec![Expr::Int(2)]
        }
    );
}

#[test]
fn app_parsing_3() {
    let code = "f x.(1) 2";
    let tokens = crate::lexer::tokenize(code).unwrap();
    let expr = parse(&tokens).unwrap();
    assert_eq!(
        expr,
        Expr::App {
            fun: Box::new(Expr::Var("f".to_string())),
            args: vec![
                Expr::Get(Box::new(Expr::Var("x".to_string())), Box::new(Expr::Int(1))),
                Expr::Int(2),
            ]
        }
    );
}

#[test]
fn app_parsing_4() {
    let code = "let a = Array.make 1 2 in 3";
    let tokens = crate::lexer::tokenize(code).unwrap();
    let expr = parse(&tokens).unwrap();

    let expr_ = Expr::Let {
        id: "a".to_string(),
        rhs: Box::new(Expr::Array(Box::new(Expr::Int(1)), Box::new(Expr::Int(2)))),
        body: Box::new(Expr::Int(3)),
    };

    assert_eq!(expr, expr_);
}
