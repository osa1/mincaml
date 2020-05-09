use crate::ctx::{Ctx, VarId};
use crate::knormal;
use crate::knormal::{BinOp, FloatBinOp, IntBinOp};
use crate::parser::Cmp;
use crate::var::CompilerPhase::ClosureConvert;
use crate::var::Uniq;

// Used when debugging
#[allow(unused_imports)]
use crate::utils;

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
    // Assign return value to this variable and jump to the label. Used when lowering let bindings.
    Asgn(VarId, Label),
}

impl BlockSequel {
    fn get_ret_var(&self, ctx: &mut CcCtx) -> VarId {
        use BlockSequel::*;
        match self {
            Asgn(var, _) => *var,
            Return => ctx.fresh_var(),
        }
    }
}

impl Block {
    fn new(label: Label, mut stmts: Vec<Asgn>, sequel: BlockSequel, value: Atom) -> Block {
        let exit = match sequel {
            BlockSequel::Return => Exit::Return(value),
            BlockSequel::Asgn(lhs, label) => {
                match value {
                    // TODO: Should we handle this case in the call site? Or make it impossible to
                    // happen somehow?
                    Atom::Var(rhs) if lhs == rhs => {}
                    _ => {
                        stmts.push(Asgn { lhs, rhs: Expr::Atom(value) });
                    }
                }
                Exit::Jump(label)
            }
        };

        Block { label, stmts, exit }
    }
}

// Exit nodes of basic blocks
#[derive(Debug, PartialEq)]
pub enum Exit {
    Return(Atom),
    Branch { v1: VarId, v2: VarId, cond: Cmp, then_label: Label, else_label: Label },
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
    App(VarId, Vec<VarId>),
    ExtApp(String, Vec<VarId>),
    // Tuple allocation
    Tuple(Vec<VarId>), // TODO: Lower this more?
    // Tuple field read
    TupleIdx(VarId, usize),
    // Array field read
    Get(VarId, Atom),
    // Array field write
    Put(VarId, Atom, Atom),
}

#[derive(Debug, PartialEq)]
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

// Returns whether the added block was a fork (i.e. then or else branch of an if)
fn cc_block(
    ctx: &mut CcCtx, blocks: &mut Vec<Block>, label: Label, mut stmts: Vec<Asgn>,
    sequel: BlockSequel, expr: knormal::Expr,
) -> bool {
    match expr {
        knormal::Expr::Unit => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Unit));
            false
        }

        knormal::Expr::Int(i) => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Int(i)));
            false
        }

        knormal::Expr::Float(f) => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Float(f)));
            false
        }

        knormal::Expr::Neg(var) => {
            let tmp = ctx.fresh_var();
            stmts.push(Asgn { lhs: tmp, rhs: Expr::Neg(var) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(tmp)));
            false
        }

        knormal::Expr::FNeg(var) => {
            let tmp = ctx.fresh_var();
            stmts.push(Asgn { lhs: tmp, rhs: Expr::FNeg(var) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(tmp)));
            false
        }

        knormal::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx);
            stmts.push(Asgn { lhs: tmp, rhs: Expr::IBinOp(BinOp { op, arg1, arg2 }) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(tmp)));
            false
        }

        knormal::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx);
            stmts.push(Asgn { lhs: tmp, rhs: Expr::FBinOp(BinOp { op, arg1, arg2 }) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(tmp)));
            false
        }

        knormal::Expr::If(v1, v2, cmp, e1, e2) => {
            let then_label = ctx.fresh_label();
            let else_label = ctx.fresh_label();
            blocks.push(Block {
                label,
                stmts,
                exit: Exit::Branch { v1, v2, cond: cmp, then_label, else_label },
            });
            cc_block(ctx, blocks, then_label, vec![], sequel.clone(), *e1);
            cc_block(ctx, blocks, else_label, vec![], sequel, *e2);
            true
        }

        knormal::Expr::Var(var) => {
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(var)));
            false
        }

        knormal::Expr::Let { id, ty: _, rhs, body } => {
            let cont_label = ctx.fresh_label();
            let rhs_sequel = BlockSequel::Asgn(id, cont_label);
            let forked = cc_block(ctx, blocks, label, stmts, rhs_sequel, *rhs);

            // Is the last block of RHS is not a forked block (i.e. then or else branch of an if)
            // then we can continue extend that block. If not we'll have to use the continuation
            // block.
            if !forked {
                let Block { label, stmts, exit } = blocks.pop().unwrap();
                assert_eq!(exit, Exit::Jump(cont_label));
                cc_block(ctx, blocks, label, stmts, sequel, *body)
            } else {
                cc_block(ctx, blocks, cont_label, vec![], sequel, *body)
            }
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
                fvs(ctx.ctx, &*rhs, &mut closure_fvs);
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
                    .push(Asgn { lhs: *fv, rhs: Expr::Get(name, Atom::Int(fv_idx as u64 + 1)) });
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
            cc_block(ctx, blocks, label, stmts, sequel, *body)
        }

        knormal::Expr::App(fun, mut args) => {
            // f(x) -> f.0(f, x)
            let fun_tmp = ctx.fresh_var();
            stmts.push(Asgn { lhs: fun_tmp, rhs: Expr::Get(fun, Atom::Int(0)) });
            args.insert(0, fun);

            let ret_tmp = sequel.get_ret_var(ctx);

            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::App(fun_tmp, args) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
            false
        }

        knormal::Expr::ExtApp(ext_fn, args) => {
            let ret_tmp = sequel.get_ret_var(ctx);
            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::ExtApp(ext_fn, args) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
            false
        }

        knormal::Expr::Tuple(args) => {
            let ret_tmp = sequel.get_ret_var(ctx);
            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::Tuple(args) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
            false
        }

        knormal::Expr::TupleIdx(tuple, idx) => {
            let ret_tmp = sequel.get_ret_var(ctx);
            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::TupleIdx(tuple, idx) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
            false
        }

        knormal::Expr::Get(array, idx) => {
            let ret_tmp = sequel.get_ret_var(ctx);
            stmts.push(Asgn { lhs: ret_tmp, rhs: Expr::Get(array, Atom::Var(idx)) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
            false
        }

        knormal::Expr::Put(array, idx, val) => {
            let ret_tmp = sequel.get_ret_var(ctx);
            stmts
                .push(Asgn { lhs: ret_tmp, rhs: Expr::Put(array, Atom::Var(idx), Atom::Var(val)) });
            blocks.push(Block::new(label, stmts, sequel, Atom::Var(ret_tmp)));
            false
        }
    }
}

use std::fmt;

fn print_comma_sep<A>(
    ctx: &Ctx, stuffs: &mut dyn Iterator<Item = &A>,
    show_stuff: fn(&A, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error>,
    w: &mut dyn fmt::Write,
) -> Result<(), fmt::Error> {
    let mut add_comma = false;
    for stuff in stuffs {
        if add_comma {
            write!(w, ", ")?;
        } else {
            add_comma = true;
        }
        show_stuff(stuff, ctx, w)?;
    }
    Ok(())
}

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Fun { name, entry: _, args, blocks } = self;

        write!(w, "function ")?;
        pp_id(ctx, *name, w)?;
        write!(w, "(")?;
        print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
        writeln!(w, ")")?;

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
            Branch { v1, v2, cond, then_label, else_label } => {
                write!(w, "if ")?;
                pp_id(ctx, *v1, w)?;
                write!(w, " {} ", cond)?;
                pp_id(ctx, *v2, w)?;
                write!(w, " then {} else {}", then_label, else_label)
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
            IBinOp(BinOp { op, arg1, arg2 }) => {
                pp_id(ctx, *arg1, w)?;
                let op_str = match op {
                    IntBinOp::Add => " + ",
                    IntBinOp::Sub => " - ",
                };
                write!(w, "{}", op_str)?;
                pp_id(ctx, *arg2, w)
            }
            FBinOp(BinOp { op, arg1, arg2 }) => {
                pp_id(ctx, *arg1, w)?;
                let op_str = match op {
                    FloatBinOp::Add => " +. ",
                    FloatBinOp::Sub => " -. ",
                    FloatBinOp::Mul => " *. ",
                    FloatBinOp::Div => " /. ",
                };
                write!(w, "{}", op_str)?;
                pp_id(ctx, *arg2, w)
            }
            Neg(var) => {
                write!(w, "-")?;
                pp_id(ctx, *var, w)
            }
            FNeg(var) => {
                write!(w, "-.")?;
                pp_id(ctx, *var, w)
            }
            App(fun, args) => {
                pp_id(ctx, *fun, w)?;
                write!(w, "(")?;
                print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
                write!(w, ")")
            }
            ExtApp(ext_fun, args) => {
                write!(w, "{}(", ext_fun)?;
                print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
                write!(w, ")")
            }
            Tuple(args) => {
                write!(w, "(")?;
                print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
                write!(w, ")")
            }
            TupleIdx(tuple, idx) => {
                pp_id(ctx, *tuple, w)?;
                write!(w, ".{}", idx)
            }
            Get(array, idx) => {
                pp_id(ctx, *array, w)?;
                write!(w, "(")?;
                idx.pp(ctx, w)?;
                write!(w, ")")
            }
            Put(array, idx, val) => {
                pp_id(ctx, *array, w)?;
                write!(w, "(")?;
                idx.pp(ctx, w)?;
                write!(w, ") <- ")?;
                val.pp(ctx, w)
            }
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

fn pp_id_ref(id: &VarId, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
    write!(w, "{}", ctx.get_var(*id))
}

fn fvs(ctx: &Ctx, e: &knormal::Expr, acc: &mut FxHashSet<VarId>) {
    use knormal::Expr::*;
    match e {
        Unit | Int(_) | Float(_) => {}
        IBinOp(BinOp { arg1, arg2, op: _ }) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        FBinOp(BinOp { arg1, arg2, op: _ }) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        Neg(arg) | FNeg(arg) => {
            fv(ctx, *arg, acc);
        }
        If(arg1, arg2, _, e1, e2) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
            fvs(ctx, e1, acc);
            fvs(ctx, e2, acc);
        }
        Let { id, ty: _, rhs, body } => {
            fvs(ctx, rhs, acc);
            fvs(ctx, body, acc);
            acc.remove(id);
        }
        Var(id) => {
            fv(ctx, *id, acc);
        }
        LetRec { name, ty: _, args, rhs, body } => {
            fvs(ctx, rhs, acc);
            fvs(ctx, body, acc);
            acc.remove(name);
            for arg in args {
                acc.remove(arg);
            }
        }
        App(fun, args) => {
            fv(ctx, *fun, acc);
            for arg in args {
                fv(ctx, *arg, acc);
            }
        }
        ExtApp(_, args) | Tuple(args) => {
            for arg in args {
                fv(ctx, *arg, acc);
            }
        }
        TupleIdx(arg, _) => {
            fv(ctx, *arg, acc);
        }
        Get(arg1, arg2) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        Put(arg1, arg2, arg3) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
            fv(ctx, *arg3, acc);
        }
    }
}

fn fv(ctx: &Ctx, var: VarId, acc: &mut FxHashSet<VarId>) {
    if !ctx.is_builtin_var(var) {
        acc.insert(var);
    }
}
