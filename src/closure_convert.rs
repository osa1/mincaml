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
    name: VarId,
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
    // Branch {
    //     cond: VarId,
    //     then_label: Label,
    //     else_label: Label,
    // },
    Jump(Label),
    // Assign return value to this variable and jump to the label. Used when lowering let bindings.
    Asgn(VarId, Label),
}

impl BlockSequel {
    fn get_ret_var(&self) -> Option<VarId> {
        use BlockSequel::*;
        match self {
            Return | Jump(_) => None,
            Asgn(var, _) => Some(*var),
        }
    }
}

impl Block {
    fn new(label: Label, mut stmts: Vec<Asgn>, sequel: BlockSequel, value: Atom) -> Block {
        let exit = match sequel {
            BlockSequel::Return => Exit::Return(value),
            BlockSequel::Jump(label) => Exit::Jump(label),
            BlockSequel::Asgn(lhs, label) => {
                stmts.push(Asgn { lhs, rhs: Expr::Atom(value) });
                Exit::Jump(label)
            }
        };

        Block { label, stmts, exit }
    }
}

// Exit nodes of basic blocks
#[derive(Debug)]
pub enum Exit {
    Return(Atom),
    Branch { cond: VarId, then_label: Label, else_label: Label },
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
    Get(VarId, Atom),
    Put(VarId, Atom, Atom),
}

#[derive(Debug)]
pub enum Atom {
    Unit,
    Int(u64),
    Float(f64),
    Var(VarId),
}

// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    // Functions generated so far
    funs: Vec<Fun>,
}

impl<'ctx> CcCtx<'ctx> {
    fn fresh_var(&mut self) -> VarId {
        self.ctx.fresh_generated_var(ClosureConvert)
    }

    fn fresh_label(&mut self) -> Label {
        self.ctx.fresh_label()
    }
}

pub fn closure_convert(ctx: &mut Ctx, expr: knormal::Expr) -> Vec<Fun> {
    let mut cc_ctx = CcCtx { ctx, funs: vec![] };

    let main_name = cc_ctx.fresh_var();
    let mut main_blocks = vec![];
    let entry_label = cc_ctx.fresh_label();
    cc_block(
        &mut cc_ctx,
        &mut main_blocks,
        entry_label,
        vec![],
        BlockSequel::Return,
        expr,
    );

    cc_ctx.funs.push(Fun {
        name: main_name,
        entry: entry_label,
        args: vec![],
        blocks: main_blocks,
    });

    cc_ctx.funs
}

fn cc_block(
    ctx: &mut CcCtx, blocks: &mut Vec<Block>, label: Label, mut stmts: Vec<Asgn>,
    sequel: BlockSequel, expr: knormal::Expr,
) {
    eprintln!("cc_block e={:#?}", expr);
    eprintln!("cc_block sequel={:#?}", sequel);

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
            stmts.push(Asgn { lhs: tmp, rhs: Expr::Neg(var) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(tmp)));
        }

        knormal::Expr::IfEq(v1, v2, e1, e2) => {
            let cond_var = ctx.fresh_var();
            stmts.push(Asgn { lhs: cond_var, rhs: Expr::Eq(v1, v2) });
            let then_label = ctx.fresh_label();
            let else_label = ctx.fresh_label();
            blocks.push(Block {
                label,
                stmts,
                exit: Exit::Branch { cond: cond_var, then_label, else_label },
            });
            cc_block(ctx, blocks, then_label, vec![], sequel.clone(), *e1);
            cc_block(ctx, blocks, else_label, vec![], sequel, *e2);
        }

        knormal::Expr::Let { id, ty: _, rhs, body } => {
            // TODO: This is quite inefficient; when we don't fork in the RHS we should be able
            // continue using the current block.
            let cont_label = ctx.fresh_label();
            let rhs_sequel = BlockSequel::Asgn(id, cont_label);
            cc_block(ctx, blocks, label, stmts, rhs_sequel, *rhs);
            cc_block(ctx, blocks, cont_label, vec![], sequel, *body);
        }

        knormal::Expr::LetRec { name, ty: _, mut args, rhs, body } => {
            // TODO: Not sure about reusing 'name' in multiple places below.

            // After cc 'name' will refer to the closure tuple. For the function we'll need a fresh
            // variable.
            let fun_var = ctx.fresh_var();

            // Free variables of the closure will be moved to tuple payload
            // NOTE: An inefficiency here is that if we have deeply nested letrecs we'll be
            // computing fvs of nested letrecs when computing the outer ones. One solution could be
            // to annotate LetRecs with fvs. Doesn't matter in practice though.
            let closure_fvs: Vec<VarId> = {
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

            // Emit function
            args.insert(0, name); // first argument will be 'self'
            let fun_entry_label = ctx.fresh_label();
            let mut fun_blocks = vec![];
            let mut entry_block_stmts = vec![];
            // Bind captured variables in function body
            for (fv_idx, fv) in closure_fvs.iter().enumerate() {
                entry_block_stmts
                    .push(Asgn { lhs: *fv, rhs: Expr::Get(name, Atom::Int(fv_idx as u64)) });
            }
            cc_block(
                ctx,
                &mut fun_blocks,
                fun_entry_label,
                entry_block_stmts,
                BlockSequel::Return,
                *rhs,
            );
            ctx.funs
                .push(Fun { name: fun_var, entry: fun_entry_label, args, blocks: fun_blocks });

            // Body
            let mut closure_tuple_args = closure_fvs;
            closure_tuple_args.insert(0, fun_var);
            let tuple_expr = Expr::Tuple(closure_tuple_args);
            stmts.push(Asgn { lhs: name, rhs: tuple_expr });
            cc_block(ctx, blocks, label, stmts, sequel, *body);
        }

        knormal::Expr::App(fun, mut args) => {
            // f(x) -> f.0(f, x)
            let fun_tmp = ctx.fresh_var();
            stmts.push(Asgn { lhs: fun_tmp, rhs: Expr::Get(fun, Atom::Int(0)) });
            args.insert(0, fun);

            let ret_tmp = sequel.get_ret_var().unwrap_or_else(|| ctx.fresh_var());

            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::App(fun_tmp, args) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
        }

        knormal::Expr::ExtApp(ext_fn, args) => {
            let ret_tmp = sequel.get_ret_var().unwrap_or_else(|| ctx.fresh_var());
            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::ExtApp(ext_fn, args) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
        }

        _ => todo!(),
    }
}

use std::fmt;

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Fun { name, entry: _, args, blocks } = self;

        writeln!(w, "function ")?;
        pp_id(ctx, *name, w)?;
        writeln!(w, " args: {:?}", args)?;

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
            Branch { cond, then_label, else_label } => {
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
        Let { id, ty: _, rhs, body } => {
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(id);
        }
        Var(id) => {
            acc.insert(id.clone());
        }
        LetRec { name, ty: _, args, rhs, body } => {
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
