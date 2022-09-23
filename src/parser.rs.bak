use crate::ast::ParsedExpr;
use crate::common::{Cmp, FloatBinOp, IntBinOp};
use crate::lexer::Token;

use parsegen::parser;

parser! {
    enum Token {
        "(" => Token::LParen, // 0
        ")" => Token::RParen,
        "bool" => Token::Bool(<bool>),
        "not" => Token::Not,
        "if" => Token::If,
        "then" => Token::Then, // 5
        "else" => Token::Else,
        "let" => Token::Let,
        "rec" => Token::Rec,
        "in" => Token::In,
        "-" => Token::Minus, // 10
        "-." => Token::MinusDot,
        "+" => Token::Plus,
        "+." => Token::PlusDot,
        "*." => Token::AstDot,
        "/." => Token::SlashDot, // 15
        "=" => Token::Equal,
        "<>" => Token::LessGreater,
        "<=" => Token::LessEqual,
        "<-" => Token::LessMinus,
        "<" => Token::Less, // 20
        ">=" => Token::GreaterEqual,
        ">" => Token::Greater,
        "." => Token::Dot,
        "," => Token::Comma,
        ";" => Token::Semicolon, // 25
        "_" => Token::Underscore,
        "Array.create" => Token::ArrayCreate,
        "id" => Token::Id(<String>), // 28
        "int" => Token::Int(<i64>),
        "float" => Token::Float(<f64>), // 30
    }

    // Entry point
    pub Expr: ParsedExpr = {
        <expr:SeqExpr> =>
            expr,
    };

    // `;`
    SeqExpr: ParsedExpr = {
        <expr:LetExpr> =>
            expr,

        // Making it right associative. It works either way.
        <expr1:LetExpr> ";" <expr2:SeqExpr> =>
            // TODO: fix the binder
            ParsedExpr::Let { bndr: "_".to_string(), rhs: Box::new(expr1), body: Box::new(expr2) },
    };

    // `let`, `let rec`, and `let (...)`
    LetExpr: ParsedExpr = {
        <expr:IfExpr> =>
            expr,

        "let" <bndr:Binder> "=" <rhs:SeqExpr> "in" <body:SeqExpr> =>
            ParsedExpr::Let { bndr, rhs: Box::new(rhs), body: Box::new(body) },

        "let" "rec" <bndr:Binder> <args:Binders1> "=" <rhs:SeqExpr> "in" <body:SeqExpr> =>
            ParsedExpr::LetRec { bndr, args, rhs: Box::new(rhs), body: Box::new(body) },

        "let" "(" <bndr:Binder> <mut bndrs:CommaBinder1_Rev> ")" "=" <rhs:SeqExpr> "in" <body:SeqExpr> => {
            bndrs.push(bndr);
            bndrs.reverse();
            ParsedExpr::LetTuple {
                bndrs,
                rhs: Box::new(rhs),
                body: Box::new(body),
            }
        },
    };

    Binder: String = {
        "_" =>
            "_".to_owned(),

        <id:"id"> =>
            id,
    };

    // One or more ", id", returned in reverse
    CommaBinder1_Rev: Vec<String> = {
        "," <bndr:Binder> <mut bndrs:CommaBinder0_Rev> => {
            bndrs.push(bndr);
            bndrs
        }
    };

    CommaBinder0_Rev: Vec<String> = {
        => vec![],

        "," <bndr:Binder> <mut bndrs:CommaBinder0_Rev> => {
            bndrs.push(bndr);
            bndrs
        }
    };

    // One or more identifiers
    Binders1: Vec<String> = {
        <bndr:Binder> <mut bndrs:Binders0_Rev> => {
            bndrs.push(bndr);
            bndrs.reverse();
            bndrs
        }
    };

    // Zero or more identifiers
    Binders0_Rev: Vec<String> = {
        => vec![],

        // TODO: I don't understand why I have to inline Binder here.
        <bndr:"id"> <mut bndrs:Binders0_Rev> => {
            bndrs.push(bndr);
            bndrs
        }

        "_" <mut bndrs:Binders0_Rev> => {
            bndrs.push("_".to_owned());
            bndrs
        }
    };

    // if-then-else
    IfExpr: ParsedExpr = {
        <expr:TupleExpr> =>
            expr,

        "if" <e1:SeqExpr> "then" <e2:SeqExpr> "else" <e3:LetExpr> =>
            ParsedExpr::If(Box::new(e1), Box::new(e2), Box::new(e3)),
    };

    TupleExpr: ParsedExpr = {
        <expr:CmpOpExpr> =>
            expr,

        <expr:CmpOpExpr> <mut exprs:CommaExpr1> => {
            exprs.push(expr);
            exprs.reverse();
            ParsedExpr::Tuple(exprs)
        },
    };

    // One or more ", <expr>"
    CommaExpr1: Vec<ParsedExpr> = {
        "," <expr:CmpOpExpr> <mut exprs:CommaExpr0> => {
            exprs.push(expr);
            exprs
        },
    };

    // Zero or more ", <expr>"
    CommaExpr0: Vec<ParsedExpr> = {
        => vec![],

        "," <expr:CmpOpExpr> <mut exprs:CommaExpr0> => {
            exprs.push(expr);
            exprs
        },
    };

    // Comparison operators: `=`, `<>`, `<=` `<`, `>=`, `>`. These all have the same the same
    // precedence, and are all left associative.
    CmpOpExpr: ParsedExpr = {
        <expr:BinOp1Expr> =>
            expr,

        <expr1:CmpOpExpr> <op:CmpOp> <expr2:BinOp1Expr> =>
            ParsedExpr::Cmp(Box::new(expr1), op, Box::new(expr2)),
    };

    CmpOp: Cmp = {
        "=" => Cmp::Equal,
        "<>" => Cmp::NotEqual,
        "<" => Cmp::LessThan,
        "<=" => Cmp::LessThanOrEqual,
        ">" => Cmp::GreaterThan,
        ">=" => Cmp::GreaterThanOrEqual,
    };

    // `+` and `-`
    BinOp1Expr: ParsedExpr = {
        <expr:BinOp2Expr> =>
            expr,

        // Left associative
        <expr1:BinOp1Expr> "-" <expr2:BinOp2Expr> =>
            ParsedExpr::IntBinOp(Box::new(expr1), IntBinOp::Sub, Box::new(expr2)),

        // Left associative
        <expr1:BinOp1Expr> "+" <expr2:BinOp2Expr> =>
            ParsedExpr::IntBinOp(Box::new(expr1), IntBinOp::Add, Box::new(expr2)),

        // Left associative
        <expr1:BinOp1Expr> "+." <expr2:BinOp2Expr> =>
            ParsedExpr::FloatBinOp(Box::new(expr1), FloatBinOp::Add, Box::new(expr2)),

        // Left associative
        <expr1:BinOp1Expr> "-." <expr2:BinOp2Expr> =>
            ParsedExpr::FloatBinOp(Box::new(expr1), FloatBinOp::Sub, Box::new(expr2)),
    };

    // `*.` and `/.`
    BinOp2Expr: ParsedExpr = {
        <expr:UnOpExpr> =>
            expr,

        // Left associative
        <expr1:BinOp2Expr> "*." <expr2:UnOpExpr> =>
            ParsedExpr::FloatBinOp(Box::new(expr1), FloatBinOp::Mul, Box::new(expr2)),

        // Left associative
        <expr1:BinOp2Expr> "/." <expr2:UnOpExpr> =>
            ParsedExpr::FloatBinOp(Box::new(expr1), FloatBinOp::Div, Box::new(expr2)),
    };

    UnOpExpr: ParsedExpr = {
        "-" <expr:AppExpr> =>
            match expr {
                ParsedExpr::Float(f) =>
                    ParsedExpr::FNeg(Box::new(ParsedExpr::Float(f))),
                other =>
                    ParsedExpr::Neg(Box::new(other)),
            },

        "-." <expr:AppExpr> =>
            ParsedExpr::FNeg(Box::new(expr)),

        "not" <expr:AppExpr> =>
            ParsedExpr::Not(Box::new(expr)),

        <expr:AppExpr> =>
            expr,
    };

    AppExpr: ParsedExpr = {
        <expr:GetPutExpr> =>
            expr,

        <expr1:GetPutExpr> <exprs:GetPutExprs1> =>
            ParsedExpr::App { fun: Box::new(expr1), args: exprs },

        "Array.create" <e1:GetPutExpr> <e2:GetPutExpr> =>
            ParsedExpr::Array { len: Box::new(e1), elem: Box::new(e2) },
    };

    // Array get and put expressions: `<expr> . ( <expr> )`, `<expr> . ( <expr> ) <- `<expr>`
    GetPutExpr: ParsedExpr = {
        <expr1:SimpleExpr> =>
            expr1,

        <expr1:SimpleExpr> <get_put_suffix:GetOrPutSuffix1_Rev> => {
            let mut expr = expr1;
            let (mut gets, put) = get_put_suffix;

            match put {
                None => {
                    for idx in gets.into_iter().rev() {
                        expr = ParsedExpr::Get(Box::new(expr), Box::new(idx));
                    }
                }
                Some(put) => {
                    let put_idx = gets.remove(0);
                    for idx in gets.into_iter().rev() {
                        expr = ParsedExpr::Get(Box::new(expr), Box::new(idx));
                    }
                    expr = ParsedExpr::Put(Box::new(expr), Box::new(put_idx), Box::new(put));
                }
            }

            expr
        }
    };

    GetOrPutSuffix1_Rev: (Vec<ParsedExpr>, Option<ParsedExpr>) = {
        "." "(" <expr:SeqExpr> ")" <mut rest:GetOrPutSuffix0_Rev> => {
            let (mut gets, put) = rest;
            gets.push(expr);
            (gets, put)
        },
    };

    GetOrPutSuffix0_Rev: (Vec<ParsedExpr>, Option<ParsedExpr>) = {
        => (vec![], None),

        "." "(" <expr:SeqExpr> ")" <mut rest:GetOrPutSuffix0_Rev> => {
            let (mut gets, put) = rest;
            gets.push(expr);
            (gets, put)
        },

        "<-" <expr:LetExpr> =>
            (vec![], Some(expr)),
    };

    // One or more exprs
    GetPutExprs1: Vec<ParsedExpr> = {
        <expr:GetPutExpr> <mut exprs:GetPutExprs0> => {
            exprs.push(expr);
            exprs.reverse();
            exprs
        }
    };

    // Zero or more exprs
    GetPutExprs0: Vec<ParsedExpr> = {
        => vec![],

        <expr1:GetPutExpr> <mut exprs:GetPutExprs0> => {
            exprs.push(expr1);
            exprs
        },
    };


    SimpleExpr: ParsedExpr = {
        "(" ")" =>
            ParsedExpr::Unit,

        "(" <expr:SeqExpr> ")" =>
            expr,

        <b:"bool"> =>
            ParsedExpr::Bool(b),

        <i:"int"> =>
            ParsedExpr::Int(i),

        <f:"float"> =>
            ParsedExpr::Float(f),

        <var:"id"> =>
            ParsedExpr::Var(var),
    };
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::lexer::tokenize;

    fn parse(s: &str) -> ParsedExpr {
        let tokens = tokenize(s).unwrap();
        Expr::parse(tokens.into_iter().map(|r| Ok::<_, ()>(r))).unwrap()
    }

    #[test]
    fn test_binop_1() {
        assert_eq!(
            parse("1 - 2"),
            ParsedExpr::IntBinOp(
                Box::new(ParsedExpr::Int(1)),
                IntBinOp::Sub,
                Box::new(ParsedExpr::Int(2))
            )
        );
        assert_eq!(
            parse("1 2"),
            ParsedExpr::App {
                fun: Box::new(ParsedExpr::Int(1)),
                args: vec![ParsedExpr::Int(2)]
            }
        );
        assert_eq!(
            parse("- 1 2"),
            ParsedExpr::Neg(Box::new(ParsedExpr::App {
                fun: Box::new(ParsedExpr::Int(1)),
                args: vec![ParsedExpr::Int(2)],
            }))
        );
        assert_eq!(
            parse("- 1 - 2"),
            ParsedExpr::IntBinOp(
                Box::new(ParsedExpr::Neg(Box::new(ParsedExpr::Int(1)))),
                IntBinOp::Sub,
                Box::new(ParsedExpr::Int(2))
            )
        );
        assert_eq!(
            parse("1 - - 2"),
            ParsedExpr::IntBinOp(
                Box::new(ParsedExpr::Int(1)),
                IntBinOp::Sub,
                Box::new(ParsedExpr::Neg(Box::new(ParsedExpr::Int(2))))
            )
        );
    }
}
