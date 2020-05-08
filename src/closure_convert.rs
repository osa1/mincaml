#![allow(dead_code)]

use crate::ctx::{Ctx, VarId};
use crate::knormal;
use crate::knormal::{BinOp, FloatBinOp, IntBinOp};
use crate::var::CompilerPhase::ClosureConvert;
use crate::var::Uniq;

use fxhash::FxHashSet;

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

#[derive(Debug)]
pub struct OpenBlock {
    label: Label,
    stmts: Vec<Asgn>,
}

// Basic blocks
#[derive(Debug)]
pub struct Block {
    label: Label,
    stmts: Vec<Asgn>,
    exit: Exit,
}

// Assignments
#[derive(Debug)]
pub struct Asgn {
    lhs: VarId,
    rhs: Expr,
}

#[derive(Debug, Clone)]
pub enum BlockSequel {
    Return,
    Branch {
        cond: VarId,
        then_label: Label,
        else_label: Label,
    },
    Jump(Label),
    // Assign return value to this variable
    Asgn(VarId),
}

impl Block {
    fn new(label: Label, stmts: Vec<Asgn>, sequel: BlockSequel, value: Atom) -> Block {
        let exit = match sequel {
            BlockSequel::Return => Exit::Return(value),
            BlockSequel::Branch {
                cond,
                then_label,
                else_label,
            } => Exit::Branch {
                cond,
                then_label,
                else_label,
            },
            BlockSequel::Jump(label) => Exit::Jump(label),
            BlockSequel::Asgn(_) => panic!("Assignment in block exit"),
        };

        Block { label, stmts, exit }
    }
}

// Exit nodes of basic blocks
#[derive(Debug)]
pub enum Exit {
    Return(Atom),
    Branch {
        cond: VarId,
        then_label: Label,
        else_label: Label,
    },
    Jump(Label),
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
    todo!()
    // let mut ctx = CcCtx::new(ctx, main_label);
    // let _ = cc_expr(&mut ctx, expr);
    // let funs = ctx.finish();
    // Program { main_label, funs }
}

// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    // Functions generated so far
    funs: Vec<Fun>,
    current_fun: Fun,
}

impl<'ctx> CcCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx, current_fun: Fun) -> CcCtx<'ctx> {
        CcCtx {
            ctx,
            funs: vec![],
            current_fun,
        }
    }

    fn fresh_var(&mut self) -> VarId {
        self.ctx.fresh_generated_var(ClosureConvert)
    }

    fn fresh_label(&mut self) -> Label {
        self.ctx.fresh_label()
    }
}

fn cc_function(
    ctx: &mut CcCtx,
    label: Label,
    args: Vec<VarId>,
    mut blocks: Vec<Block>,
    expr: knormal::Expr,
) {
    cc_block(ctx, &mut blocks, label, vec![], BlockSequel::Return, expr);
    ctx.funs.push(Fun {
        entry: label,
        args,
        blocks,
    });
}

fn cc_block(
    ctx: &mut CcCtx,
    blocks: &mut Vec<Block>,
    label: Label,
    mut stmts: Vec<Asgn>,
    sequel: BlockSequel,
    expr: knormal::Expr,
) {
    match expr {
        knormal::Expr::Unit => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Unit));
        }

        knormal::Expr::Int(i) => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Int(i)));
        }

        knormal::Expr::Float(f) => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Float(f)));
        }

        knormal::Expr::Neg(var) => {
            let tmp = ctx.fresh_var();
            stmts.push(Asgn {
                lhs: tmp,
                rhs: Expr::Neg(var),
            });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(tmp)));
        },

        knormal::Expr::IfEq(v1, v2, e1, e2) => {
            let cond_var = ctx.fresh_var();
            stmts.push(Asgn {
                lhs: cond_var,
                rhs: Expr::Eq(v1, v2),
            });
            let then_label = ctx.fresh_label();
            let else_label = ctx.fresh_label();
            let cont_label = ctx.fresh_label();
            blocks.push(Block {
                label,
                stmts,
                exit: Exit::Branch {
                    cond: cond_var,
                    then_label,
                    else_label,
                },
            });
            cc_block(ctx, blocks, then_label, vec![], BlockSequel::Jump(cont_label), *e1);
            cc_block(ctx, blocks, else_label, vec![], BlockSequel::Jump(cont_label), *e2);
            // TODO: When we need to extend the block (e.g. when generating code for a let binding)
            // we need to create a continuation block here and return it as extension point.
            todo!()
        }

        /*
        knormal::Expr::Let { id, ty, rhs, body } => {
            TODO: Need to be able to extend the block after let binding.
            cc_block(ctx, blocks, label, stmts, BlockSequel::Asgn(id), *rhs);
        }
        */

        _ => todo!(),
    }
}

/*
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

        knormal::Expr::Var(var) => Atom::Var(var),

        //
        // Closure conversion
        //
        knormal::Expr::LetRec {
            name,
            ty,
            args,
            rhs,
            body,
        } => {
            // After cc 'name' will refer to the closure tuple. For the function we'll need a fresh
            // variable.
            let fun_var = ctx.fresh_var();

            // Free variables of the closure will be moved to tuple payload
            // NOTE: An inefficiency here is that if we have deeply nested letrecs we'll be
            // computing fvs of nested letrecs when computing the outer ones. One solution could be
            // to annotate LetRecs with fvs. Doesn't matter in practice though.
            let mut closure_tuple_args: Vec<VarId> = {
                let mut closure_fvs: FxHashSet<VarId> = Default::default();
                fvs(&*rhs, &mut closure_fvs);
                for arg in &args {
                    closure_fvs.remove(arg);
                }
                closure_fvs.into_iter().collect()
            };

            // In the RHS and the body, 'name' will refer to the tuple. However in the RHS the
            // tuple will be the first argument of the function, in the body we'll allocate a
            // tuple.

            // Body
            closure_tuple_args.insert(0, fun_var);
            let tuple_expr = Expr::Tuple(closure_tuple_args);
            ctx.add_asgn(name, tuple_expr);
            let body_ret = cc_expr(ctx, *body);

            // Emit a function for the RHS


            body_ret
        }

        knormal::Expr::App(fun, args) => todo!(),

        //
        // End of closure conversion
        //
        _ => todo!(),
    }
}
*/

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
            Return(atom) => {
                write!(w, "return ")?;
                atom.pp(ctx, w)
            }
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
        }
    }
}

impl Asgn {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
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
