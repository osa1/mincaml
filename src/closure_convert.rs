use crate::ctx::{Ctx, VarId};
use crate::knormal;
use crate::knormal::{BinOp, FloatBinOp, IntBinOp};
use crate::var::CompilerPhase::ClosureConvert;
use crate::var::Uniq;

use std::mem::replace;

pub type Label = Uniq; // FIXME how to represent this best?

#[derive(Debug)]
pub struct Program {
    pub main_label: Label,
    pub funs: Vec<Fun>,
}

// Functions
#[derive(Debug)]
pub struct Fun {
    entry: Label,
    args: Vec<VarId>,
    blocks: Vec<Block>,
}

// Basic blocks
#[derive(Debug)]
pub struct Block {
    label: Label,
    stmts: Vec<Asgn>,
    exit: Exit,
}

// Exit nodes of basic blocks
#[derive(Debug)]
pub enum Exit {
    Return,
    Branch {
        cond: VarId,
        then_label: Label,
        else_label: Label,
    },
    Jump(Label),
    Call {
        fun: VarId,
        args: Vec<VarId>,
    },
}

// Assignments
#[derive(Debug)]
pub struct Asgn {
    lhs: VarId,
    rhs: Expr,
}

// Assignment right-hand sides
#[derive(Debug)]
pub enum Expr {
    Atom(Atom),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(VarId),
    FNeg(VarId),
    Eq(VarId, VarId),
    LE(VarId, VarId),
    App(VarId, Vec<VarId>),
    ExtApp(String, Vec<VarId>),
    Tuple(Vec<VarId>), // TODO: Lower this more?
    TupleIdx(VarId, usize),
    Get(VarId, VarId),
    Put(VarId, VarId, VarId),
}

#[derive(Debug)]
pub enum Atom {
    Unit,
    Int(u64),
    Float(f64),
    Var(VarId),
}

pub fn closure_convert(ctx: &mut Ctx, expr: knormal::Expr) -> Program {
    let main_label = ctx.fresh_label();
    let mut ctx = CcCtx::new(ctx, main_label);
    let _ = cc_expr(&mut ctx, expr);
    let funs = ctx.finish();
    Program { main_label, funs }
}

// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    funs: Vec<Fun>,
    current_fun_label: Label,
    current_fun_args: Vec<VarId>,
    current_fun_blocks: Vec<Block>,
    current_block_label: Label,
    current_block: Vec<Asgn>,
}

impl<'ctx> CcCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx, main_label: Label) -> CcCtx<'ctx> {
        let entry_label = ctx.fresh_label();
        CcCtx {
            ctx,
            funs: vec![],
            current_fun_label: main_label,
            current_fun_args: vec![],
            current_fun_blocks: vec![],
            current_block_label: entry_label,
            current_block: vec![],
        }
    }

    fn fresh_var(&mut self) -> VarId {
        self.ctx.fresh_generated_var(ClosureConvert)
    }

    fn fresh_label(&mut self) -> Label {
        self.ctx.fresh_label()
    }

    fn add_asgn(&mut self, lhs: VarId, rhs: Expr) {
        self.current_block.push(Asgn { lhs, rhs })
    }

    fn finish_block_and_enter(&mut self, exit: Exit, next_block: Label) {
        let stmts = replace(&mut self.current_block, vec![]);
        let label = replace(&mut self.current_block_label, next_block);
        let block = Block { label, stmts, exit };
        self.current_fun_blocks.push(block);
    }

    fn finish(self) -> Vec<Fun> {
        let stmts = self.current_block;
        let label = self.current_block_label;
        let block = Block {
            label,
            stmts,
            exit: Exit::Return,
        };
        let mut blocks = self.current_fun_blocks;
        blocks.push(block);
        let args = self.current_fun_args;
        let fun_label = self.current_fun_label;
        let mut funs = self.funs;
        // FIXME: Entry is fun label?
        funs.push(Fun {
            entry: fun_label,
            args,
            blocks,
        });
        funs
    }
}

fn cc_expr(ctx: &mut CcCtx, expr: knormal::Expr) -> Atom {
    match expr {
        knormal::Expr::Unit => Atom::Unit,
        knormal::Expr::Int(i) => Atom::Int(i),
        knormal::Expr::Float(f) => Atom::Float(f),
        knormal::Expr::Neg(var) => {
            let tmp = ctx.fresh_var();
            ctx.add_asgn(tmp, Expr::Neg(var));
            Atom::Var(tmp)
        }

        knormal::Expr::Let { id, ty, rhs, body } => {
            let rhs = cc_expr(ctx, *rhs);
            ctx.add_asgn(id, Expr::Atom(rhs));
            cc_expr(ctx, *body)
        }

        knormal::Expr::IfEq(v1, v2, e1, e2) => {
            let cond = ctx.fresh_var();
            ctx.add_asgn(cond, Expr::Eq(v1, v2));

            let ret = ctx.fresh_var();

            let then_label = ctx.fresh_label();
            let else_label = ctx.fresh_label();
            let cont_label = ctx.fresh_label();

            ctx.finish_block_and_enter(
                Exit::Branch {
                    cond,
                    then_label,
                    else_label,
                },
                then_label,
            );

            let e1_ret = cc_expr(ctx, *e1);
            ctx.add_asgn(ret, Expr::Atom(e1_ret));
            ctx.finish_block_and_enter(Exit::Jump(cont_label), else_label);

            let e2_ret = cc_expr(ctx, *e2);
            ctx.add_asgn(ret, Expr::Atom(e2_ret));

            ctx.finish_block_and_enter(Exit::Jump(cont_label), cont_label);

            Atom::Var(ret)
        }

        _ => todo!(),
    }
}

use std::fmt;

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Fun {
            entry,
            args,
            blocks,
        } = self;

        writeln!(w, "function {} args: {:?}", entry, args)?;

        for block in blocks {
            block.pp(ctx, w)?;
        }
        writeln!(w)
    }
}

impl Block {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Block { label, stmts, exit } = self;
        writeln!(w, "{}:", label)?;
        for asgn in stmts {
            write!(w, "    ")?;
            asgn.pp(ctx, w)?;
            writeln!(w)?;
        }
        write!(w, "    ")?;
        exit.pp(ctx, w)?;
        writeln!(w)
    }
}

impl Exit {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        use Exit::*;
        match self {
            Return => write!(w, "return"),
            Branch {
                cond,
                then_label,
                else_label,
            } => {
                write!(w, "if ")?;
                pp_id(ctx, *cond, w)?;
                write!(w, " {} {}", then_label, else_label)
            }
            Jump(lbl) => write!(w, "jump {}", lbl),
            Call { fun, args } => todo!(),
        }
    }
}

impl Asgn {
    pub fn pp(& self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Asgn { lhs, rhs } = self;
        pp_id(ctx, *lhs, w)?;
        write!(w, " = ")?;
        rhs.pp(ctx, w)
    }
}

impl Expr {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        use Expr::*;
        match self {
            Atom(atom) => atom.pp(ctx, w),
            Neg(var) => {
                write!(w, "-")?;
                pp_id(ctx, *var, w)
            }
            FNeg(var) => {
                write!(w, "-.")?;
                pp_id(ctx, *var, w)
            }
            Eq(var1, var2) => {
                pp_id(ctx, *var1, w)?;
                write!(w, " == ")?;
                pp_id(ctx, *var2, w)
            }
            LE(var1, var2) => {
                pp_id(ctx, *var1, w)?;
                write!(w, " <= ")?;
                pp_id(ctx, *var2, w)
            }
            _ => todo!(),
        }
    }
}

impl Atom {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        use Atom::*;
        match self {
            Unit => write!(w, "()"),
            Int(i) => write!(w, "{}", i),
            Float(f) => write!(w, "{}", f),
            Var(var) => pp_id(ctx, *var, w),
        }
    }
}

fn pp_id(ctx: &Ctx, id: VarId, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
    write!(w, "{}", ctx.get_var(id))
}

/*
fn fvs(e: &knormal::Expr, acc: &mut FxHashSet<VarId>) {
    use knormal::Expr::*;
    match e {
        Unit | Int(_) | Float(_) => {}
        IBinOp(BinOp { arg1, arg2, op: _ }) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        FBinOp(BinOp { arg1, arg2, op: _ }) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        Neg(arg) | FNeg(arg) => {
            acc.insert(arg.clone());
        }
        IfEq(arg1, arg2, e1, e2) | IfLE(arg1, arg2, e1, e2) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
            fvs(e1, acc);
            fvs(e2, acc);
        }
        Let {
            id,
            ty: _,
            rhs,
            body,
        } => {
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(id);
        }
        Var(id) => {
            acc.insert(id.clone());
        }
        LetRec {
            name,
            ty: _,
            args,
            rhs,
            body,
        } => {
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(name);
            for arg in args {
                acc.remove(arg);
            }
        }
        App(fun, args) => {
            acc.insert(fun.clone());
            for arg in args {
                acc.insert(arg.clone());
            }
        }
        ExtApp(_, args) | Tuple(args) => {
            for arg in args {
                acc.insert(arg.clone());
            }
        }
        TupleIdx(arg, _) => {
            acc.insert(arg.clone());
        }
        Get(arg1, arg2) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        Put(arg1, arg2, arg3) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
            acc.insert(arg3.clone());
        }
    }
}
*/
