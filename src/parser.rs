use crate::ast::*;
use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::lexer::Token;
use crate::var::CompilerPhase;

pub fn parse(ctx: &mut Ctx, tokens: &[Token], arena: &mut ExprArena) -> Result<ExprIdx, ParseErr> {
    let mut parser = Parser::new(tokens);
    let expr = parser.expr(ctx, arena)?;
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

    pub fn expr0(
        &mut self, ctx: &mut Ctx, arena: &mut ExprArena, prec: usize,
    ) -> Result<ExprIdx, ParseErr> {
        match self.next_token()? {
            //
            // Single-token expressions
            //
            Token::Bool(bool) => {
                let b = *bool;
                self.consume();
                Ok(Expr::bool(b, arena))
            }
            Token::Int(int) => {
                let i = *int;
                self.consume();
                Ok(Expr::int(i, arena))
            }
            Token::Float(float) => {
                let f = *float;
                self.consume();
                Ok(Expr::float(f, arena))
            }
            Token::Id(id) => {
                let var = ctx.fresh_user_var(id);
                self.consume();
                Ok(Expr::var(var, arena))
            }

            //
            // Other stuff
            //
            Token::LParen => {
                self.consume();
                match self.next_token()? {
                    Token::RParen => {
                        self.consume();
                        Ok(Expr::unit(arena))
                    }
                    _ => {
                        // Parse everything until ')'
                        let expr = self.expr1(ctx, arena, INIT_PREC)?;
                        self.expect(Token::RParen, "')'")?;
                        Ok(expr)
                    }
                }
            }
            Token::Not if prec <= APP_PREC => {
                self.consume();
                Ok(Expr::not(self.expr1(ctx, arena, APP_PREC)?, arena))
            }
            Token::Minus if prec <= UNARY_MINUS_PREC => {
                self.consume();
                let expr = self.expr1(ctx, arena, UNARY_MINUS_PREC)?;
                // Hacky, but this is how the original min-caml parses this as well.
                if let ExprKind::Float(_) = arena[expr].kind {
                    Ok(Expr::fneg(expr, arena))
                } else {
                    Ok(Expr::neg(expr, arena))
                }
            }
            Token::MinusDot if prec <= UNARY_MINUS_PREC => {
                self.consume();
                Ok(Expr::fneg(self.expr1(ctx, arena, PLUS_MINUS_PREC)?, arena))
            }
            Token::ArrayCreate if prec <= APP_PREC => {
                self.consume();
                let expr1 = self.expr0(ctx, arena, APP_PREC)?;
                let expr2 = self.expr0(ctx, arena, APP_PREC)?;
                Ok(Expr::array(expr1, expr2, arena))
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
                        let rhs = self.expr1(ctx, arena, INIT_PREC)?;
                        self.expect(Token::In, "'in'")?;
                        let body = self.expr1(ctx, arena, LET_PREC)?;
                        Ok(Expr::letrec(bndr, args, rhs, body, arena))
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
                        let rhs = self.expr1(ctx, arena, INIT_PREC)?;
                        self.expect(Token::In, "in")?;
                        let body = self.expr1(ctx, arena, IN_PREC)?;
                        Ok(Expr::let_tuple(bndrs, rhs, body, arena))
                    }
                    Token::Id(var) => {
                        let bndr = ctx.fresh_user_var(var);
                        self.consume();
                        self.expect(Token::Equal, "'='")?;
                        // Parse everything until 'in'
                        let rhs = self.expr1(ctx, arena, INIT_PREC)?;
                        self.expect(Token::In, "'in'")?;
                        let body = self.expr1(ctx, arena, IN_PREC)?;
                        Ok(Expr::let_(bndr, rhs, body, arena))
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
                let e1 = self.expr1(ctx, arena, INIT_PREC)?;
                self.expect(Token::Then, "'then'")?;
                // Parse everything until 'else'
                let e2 = self.expr1(ctx, arena, INIT_PREC)?;
                self.expect(Token::Else, "'else'")?;
                let e3 = self.expr1(ctx, arena, IF_PREC)?;
                Ok(Expr::if_(e1, e2, e3, arena))
            }
            other => Err(ParseErr::Unexpected {
                seen: other.clone(),
                expected: "expression",
            }),
        }
    }

    pub fn expr1(
        &mut self, ctx: &mut Ctx, arena: &mut ExprArena, prec: usize,
    ) -> Result<ExprIdx, ParseErr> {
        let mut expr = self.expr0(ctx, arena, prec)?;
        let mut parsing_app = false;
        loop {
            match self.next_token() {
                Ok(Token::Semicolon) if prec <= SEMICOLON_PREC => {
                    self.consume();
                    let sym = ctx.fresh_generated_var(CompilerPhase::Parser);
                    let expr2 = self.expr1(ctx, arena, prec)?;
                    expr = Expr::let_(sym, expr, expr2, arena);
                }
                Ok(Token::Plus) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, PLUS_MINUS_PREC)?;
                    expr = Expr::int_binop(expr, IntBinOp::Add, expr2, arena);
                }
                Ok(Token::Minus) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, PLUS_MINUS_PREC)?;
                    expr = Expr::int_binop(expr, IntBinOp::Sub, expr2, arena);
                }
                Ok(Token::PlusDot) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, PLUS_MINUS_PREC)?;
                    expr = Expr::float_binop(expr, FloatBinOp::Add, expr2, arena);
                }
                Ok(Token::MinusDot) if prec < PLUS_MINUS_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, PLUS_MINUS_PREC)?;
                    expr = Expr::float_binop(expr, FloatBinOp::Sub, expr2, arena);
                }
                Ok(Token::AstDot) if prec < DIV_MULT_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, DIV_MULT_PREC)?;
                    expr = Expr::float_binop(expr, FloatBinOp::Mul, expr2, arena);
                }
                Ok(Token::SlashDot) if prec < DIV_MULT_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, DIV_MULT_PREC)?;
                    expr = Expr::float_binop(expr, FloatBinOp::Div, expr2, arena);
                }
                Ok(Token::Equal) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, CMP_PREC)?;
                    expr = Expr::cmp(expr, Cmp::Equal, expr2, arena);
                }
                Ok(Token::LessGreater) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, CMP_PREC)?;
                    expr = Expr::cmp(expr, Cmp::NotEqual, expr2, arena);
                }
                Ok(Token::Less) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, CMP_PREC)?;
                    expr = Expr::cmp(expr, Cmp::LessThan, expr2, arena);
                }
                Ok(Token::LessEqual) if prec < CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, CMP_PREC)?;
                    expr = Expr::cmp(expr, Cmp::LessThanOrEqual, expr2, arena);
                }
                Ok(Token::Greater) if prec <= CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, CMP_PREC)?;
                    expr = Expr::cmp(expr, Cmp::GreaterThan, expr2, arena);
                }
                Ok(Token::GreaterEqual) if prec <= CMP_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, CMP_PREC)?;
                    expr = Expr::cmp(expr, Cmp::GreaterThanOrEqual, expr2, arena);
                }
                Ok(Token::Comma) if prec <= TUPLE_PREC => {
                    self.consume();
                    let expr2 = self.expr1(ctx, arena, COMMA_PREC)?;
                    match &mut arena[expr] {
                        Expr {
                            kind: ExprKind::Tuple,
                            ref mut children,
                        } => {
                            children.push(expr2);
                        }
                        _ => {
                            expr = Expr::tuple(vec![expr, expr2], arena);
                        }
                    }
                }
                Ok(Token::Dot) if prec < DOT_PREC => {
                    self.consume();
                    self.expect(Token::LParen, "'('")?;
                    // Parse everything until ')'
                    let expr1 = self.expr1(ctx, arena, INIT_PREC)?;
                    self.expect(Token::RParen, "')'")?;
                    match self.next_token() {
                        Ok(Token::LessMinus) => {
                            self.consume();
                            let expr2 = self.expr1(ctx, arena, LESS_MINUS_PREC)?;
                            match &mut arena[expr] {
                                Expr {
                                    kind: ExprKind::App,
                                    ref mut children,
                                } if parsing_app => {
                                    let arg = children.pop().unwrap();
                                    let fun = children.pop().unwrap();
                                    expr = Expr::app(
                                        fun,
                                        vec![Expr::put(arg, expr1, expr2, arena)],
                                        arena,
                                    );
                                }
                                _ => {
                                    expr = Expr::put(expr, expr1, expr2, arena);
                                }
                            }
                        }
                        _ => match &mut arena[expr] {
                            Expr {
                                kind: ExprKind::App,
                                ref mut children,
                            } if parsing_app => {
                                let arg = children.pop().unwrap();
                                let fun = children.pop().unwrap();
                                expr = Expr::app(fun, vec![Expr::get(arg, expr1, arena)], arena);
                            }
                            _ => {
                                expr = Expr::get(expr, expr1, arena);
                            }
                        },
                    }
                }
                Ok(_) if prec <= APP_PREC => match self.expr0(ctx, arena, APP_PREC) {
                    Err(_) => {
                        break;
                    }
                    Ok(expr_) => match &mut arena[expr] {
                        Expr {
                            kind: ExprKind::App,
                            ref mut children,
                        } if parsing_app => {
                            children.push(expr_);
                        }
                        _ => {
                            parsing_app = true;
                            expr = Expr::app(expr, vec![expr_], arena);
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
    pub fn expr(&mut self, ctx: &mut Ctx, arena: &mut ExprArena) -> Result<ExprIdx, ParseErr> {
        let ret = self.expr1(ctx, arena, INIT_PREC)?;
        match self.next_token() {
            Err(_) => Ok(ret),
            Ok(next) => Err(ParseErr::Unexpected {
                seen: next.clone(),
                expected: "EOF",
            }),
        }
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

    fn expect_id(&mut self) -> Result<&str, ParseErr> {
        // NOTE: 'consume' and 'next_token' inlined below to work around borrowchk issues
        match self.tokens.get(self.tok_idx) {
            None => Err(ParseErr::EndOfInput),
            Some(Token::Id(id)) => {
                self.tok_idx += 1;
                Ok(id)
            }
            Some(other) => Err(ParseErr::Unexpected {
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
