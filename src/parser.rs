use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::lexer::Token;
use crate::var::CompilerPhase;

use std::fmt;

#[derive(Debug)]
pub enum Expr {
    // ()
    Unit,
    // true, false
    Bool(bool),
    Int(i64),
    Float(f64),
    // not <expr>
    Not(Box<Expr>),
    // - <expr>
    Neg(Box<Expr>),
    // '<expr> + <expr>' or '<expr> - <expr>'
    IntBinOp(Box<Expr>, IntBinOp, Box<Expr>),
    // -. <expr>
    FNeg(Box<Expr>),
    // A float binary operation, e.g. '<expr> +. <expr>'
    FloatBinOp(Box<Expr>, FloatBinOp, Box<Expr>),
    // Comparison, e.g. <expr> <= <expr>
    Cmp(Box<Expr>, Cmp, Box<Expr>),
    // if <expr> then <expr> else <expr>
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    // let <ident> = <expr> in <expr>
    Let { bndr: VarId, rhs: Box<Expr>, body: Box<Expr> },
    // <ident>
    Var(VarId),
    // let rec <ident> <ident>+ = <expr> in <expr>
    LetRec { bndr: VarId, args: Vec<VarId>, rhs: Box<Expr>, body: Box<Expr> },
    // <expr> <expr>+
    App { fun: Box<Expr>, args: Vec<Expr> },
    // <expr> (, <expr>)+
    Tuple(Vec<Expr>),
    // let ( <ident> (, <ident>)+ ) = <expr> in <expr>
    LetTuple { bndrs: Vec<VarId>, rhs: Box<Expr>, body: Box<Expr> },
    // Array.create <expr> <expr>
    Array { len: Box<Expr>, elem: Box<Expr> },
    // <expr> . ( <expr> )
    Get(Box<Expr>, Box<Expr>),
    // <expr> . ( <expr> ) <- <expr>
    Put(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Cmp::*;
        let s = match self {
            Equal => "=",
            NotEqual => "<>",
            LessThan => "<",
            LessThanOrEqual => "<=",
            GreaterThan => ">",
            GreaterThanOrEqual => ">=",
        };
        s.fmt(f)
    }
}

pub fn parse(ctx: &mut Ctx, tokens: &[Token]) -> Result<Expr, ParseErr> {
    let mut parser = Parser::new(tokens);
    let expr = parser.expr(ctx)?;
    Ok(expr)
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
        Parser { tokens, tok_idx: 0 }
    }

    pub fn expr0(&mut self, ctx: &mut Ctx, prec: usize) -> Result<Expr, ParseErr> {
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
                let var = ctx.fresh_user_var(id);
                self.consume();
                Ok(Expr::Var(var))
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
                        // Parse everything until ')'
                        let expr = self.expr1(ctx, INIT_PREC)?;
                        self.expect(Token::RParen, "')'")?;
                        Ok(expr)
                    }
                }
            }
            Token::Not if prec <= APP_PREC => {
                self.consume();
                Ok(Expr::Not(Box::new(self.expr1(ctx, APP_PREC)?)))
            }
            Token::Minus if prec <= UNARY_MINUS_PREC => {
                self.consume();
                let expr = self.expr1(ctx, UNARY_MINUS_PREC)?;
                match expr {
                    Expr::Float(_) =>
                    // Hacky, but this is how the original min-caml parses this as well.
                    {
                        Ok(Expr::FNeg(Box::new(expr)))
                    }
                    _ => Ok(Expr::Neg(Box::new(expr))),
                }
            }
            Token::MinusDot if prec <= UNARY_MINUS_PREC => {
                self.consume();
                Ok(Expr::FNeg(Box::new(self.expr1(ctx, PLUS_MINUS_PREC)?)))
            }
            Token::ArrayCreate if prec <= APP_PREC => {
                self.consume();
                let expr1 = self.expr0(ctx, APP_PREC)?;
                let expr2 = self.expr0(ctx, APP_PREC)?;
                Ok(Expr::Array { len: Box::new(expr1), elem: Box::new(expr2) })
            }
            Token::Let => {
                self.consume();
                match self.next_token()? {
                    Token::Rec => {
                        self.consume();
                        let bndr = self.expect_id()?;
                        let bndr = ctx.fresh_user_var(bndr);
                        let mut args = vec![];
                        loop {
                            match self.next_token()? {
                                Token::Underscore => {
                                    args.push(ctx.fresh_generated_var(CompilerPhase::Parser));
                                    self.consume();
                                }
                                Token::Id(arg) => {
                                    args.push(ctx.fresh_user_var(arg));
                                    self.consume();
                                }
                                Token::Equal => {
                                    break;
                                }
                                other => {
                                    return Err(ParseErr::Unexpected {
                                        expected: "binder or '='",
                                        seen: other.clone(),
                                    });
                                }
                            }
                        }
                        self.expect(Token::Equal, "'='")?;
                        // Parse everything until 'in'
                        let rhs = Box::new(self.expr1(ctx, INIT_PREC)?);
                        self.expect(Token::In, "'in'")?;
                        let body = Box::new(self.expr1(ctx, LET_PREC)?);
                        Ok(Expr::LetRec { bndr, args, rhs, body })
                    }
                    Token::LParen => {
                        self.consume();
                        let mut bndrs = vec![];
                        {
                            let bndr = self.expect_id()?;
                            bndrs.push(ctx.fresh_user_var(bndr));
                        }
                        while let Ok(Token::Comma) = self.next_token() {
                            self.consume();
                            {
                                let bndr = self.expect_id()?;
                                bndrs.push(ctx.fresh_user_var(bndr));
                            }
                        }
                        self.expect(Token::RParen, "')'")?;
                        self.expect(Token::Equal, "'='")?;
                        // Parse everything until '='
                        let rhs = self.expr1(ctx, INIT_PREC)?;
                        self.expect(Token::In, "in")?;
                        let body = self.expr1(ctx, IN_PREC)?;
                        Ok(Expr::LetTuple { bndrs, rhs: Box::new(rhs), body: Box::new(body) })
                    }
                    Token::Id(var) => {
                        let bndr = ctx.fresh_user_var(var);
                        self.consume();
                        self.expect(Token::Equal, "'='")?;
                        // Parse everything until 'in'
                        let rhs = Box::new(self.expr1(ctx, INIT_PREC)?);
                        self.expect(Token::In, "'in'")?;
                        let body = Box::new(self.expr1(ctx, IN_PREC)?);
                        Ok(Expr::Let { bndr, rhs, body })
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
                // Parse evertying until 'then'
                let e1 = self.expr1(ctx, INIT_PREC)?;
                self.expect(Token::Then, "'then'")?;
                // Parse everything until 'else'
                let e2 = self.expr1(ctx, INIT_PREC)?;
                self.expect(Token::Else, "'else'")?;
                let e3 = self.expr1(ctx, IF_PREC)?;
                Ok(Expr::If(Box::new(e1), Box::new(e2), Box::new(e3)))
            }
            other => Err(ParseErr::Unexpected { seen: other.clone(), expected: "expression" }),
        }
    }

    pub fn expr1(&mut self, ctx: &mut Ctx, prec: usize) -> Result<Expr, ParseErr> {
        let mut expr = self.expr0(ctx, prec)?;
        let mut parsing_app = false;
        loop {
            match self.next_token() {
                Ok(Token::Semicolon) if prec <= SEMICOLON_PREC => {
                    self.consume();
                    let sym = ctx.fresh_generated_var(CompilerPhase::Parser);
                    let expr2 = self.expr1(ctx, prec)?;
                    expr = Expr::Let { bndr: sym, rhs: Box::new(expr), body: Box::new(expr2) };
                }
                Ok(Token::Plus) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, PLUS_MINUS_PREC)?;
                    expr = Expr::IntBinOp(Box::new(expr), IntBinOp::Add, Box::new(expr2));
                }
                Ok(Token::Minus) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, PLUS_MINUS_PREC)?;
                    expr = Expr::IntBinOp(Box::new(expr), IntBinOp::Sub, Box::new(expr2));
                }
                Ok(Token::PlusDot) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, PLUS_MINUS_PREC)?;
                    expr = Expr::FloatBinOp(Box::new(expr), FloatBinOp::Add, Box::new(expr2));
                }
                Ok(Token::MinusDot) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, PLUS_MINUS_PREC)?;
                    expr = Expr::FloatBinOp(Box::new(expr), FloatBinOp::Sub, Box::new(expr2));
                }
                Ok(Token::AstDot) if prec < DIV_MULT_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, DIV_MULT_PREC)?;
                    expr = Expr::FloatBinOp(Box::new(expr), FloatBinOp::Mul, Box::new(expr2));
                }
                Ok(Token::SlashDot) if prec < DIV_MULT_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, DIV_MULT_PREC)?;
                    expr = Expr::FloatBinOp(Box::new(expr), FloatBinOp::Div, Box::new(expr2));
                }
                Ok(Token::Equal) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, CMP_PREC)?;
                    expr = Expr::Cmp(Box::new(expr), Cmp::Equal, Box::new(expr2));
                }
                Ok(Token::LessGreater) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, CMP_PREC)?;
                    expr = Expr::Cmp(Box::new(expr), Cmp::NotEqual, Box::new(expr2));
                }
                Ok(Token::Less) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, CMP_PREC)?;
                    expr = Expr::Cmp(Box::new(expr), Cmp::LessThan, Box::new(expr2));
                }
                Ok(Token::LessEqual) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, CMP_PREC)?;
                    expr = Expr::Cmp(Box::new(expr), Cmp::LessThanOrEqual, Box::new(expr2));
                }
                Ok(Token::Greater) if prec <= CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, CMP_PREC)?;
                    expr = Expr::Cmp(Box::new(expr), Cmp::GreaterThan, Box::new(expr2));
                }
                Ok(Token::GreaterEqual) if prec <= CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, CMP_PREC)?;
                    expr = Expr::Cmp(Box::new(expr), Cmp::GreaterThanOrEqual, Box::new(expr2));
                }
                Ok(Token::Comma) if prec <= TUPLE_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, COMMA_PREC)?;
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
                    // Parse everything until ')'
                    let expr1 = self.expr1(ctx, INIT_PREC)?;
                    self.expect(Token::RParen, "')'")?;
                    match self.next_token() {
                        Ok(Token::LessMinus) => {
                            self.consume();
                            let expr2 = self.expr1(ctx, LESS_MINUS_PREC)?;
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
                Ok(_) if prec <= APP_PREC => match self.expr0(ctx, APP_PREC) {
                    Err(_) => {
                        break;
                    }
                    Ok(expr_) => match expr {
                        Expr::App { ref mut args, .. } if parsing_app => {
                            args.push(expr_);
                        }
                        _ => {
                            parsing_app = true;
                            expr = Expr::App { fun: Box::new(expr), args: vec![expr_] };
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
    pub fn expr(&mut self, ctx: &mut Ctx) -> Result<Expr, ParseErr> {
        let ret = self.expr1(ctx, INIT_PREC)?;
        match self.next_token() {
            Err(_) => Ok(ret),
            Ok(next) => Err(ParseErr::Unexpected { seen: next.clone(), expected: "EOF" }),
        }
    }

    fn expect(&mut self, tok: Token, str: &'static str) -> Result<(), ParseErr> {
        let next_token = self.next_token()?;
        if next_token == &tok {
            self.consume();
            Ok(())
        } else {
            Err(ParseErr::Unexpected { seen: next_token.clone(), expected: str })
        }
    }

    fn expect_id(&mut self) -> Result<&str, ParseErr> {
        // NOTE: 'consume' and 'next_token' inlined below to work around borrowchk issues
        match self.tokens.get(self.tok_idx) {
            None => Err(ParseErr::EndOfInput),
            Some(Token::Id(id)) => {
                self.tok_idx += 1;
                Ok(id)
            }
            Some(other) => {
                Err(ParseErr::Unexpected { seen: other.clone(), expected: "identifier" })
            }
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
