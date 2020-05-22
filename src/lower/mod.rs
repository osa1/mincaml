#![allow(unused_imports, dead_code, unused_variables)]

mod types;

use crate::anormal;
use crate::anormal::BinOp;
use crate::cg_types::*;
use crate::ctx::*;
use crate::var::CompilerPhase;
use types::*;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug)]
struct Builder {
    current_fun_name: VarId,
    // This list is not ordered! Sort on `idx` before finalizing the function.
    finished_blocks: Vec<Block>,
    n_blocks: u32,
    current_block: Vec<Expr>,
    vals: FxHashMap<VarId, Value>,
}

#[derive(Debug)]
enum BlockSequel {
    Return,
    // Assign return value and jump to the label. Used when lowering let bindings.
    Jump(BlockIdx),
}

impl Builder {
    fn new(name: VarId, args: &[VarId]) -> Self {
        let mut vals: FxHashMap<VarId, Value> = Default::default();
        for (arg_idx, arg) in args.iter().enumerate() {
            vals.insert(*arg, Value::Arg(arg_idx));
        }

        Self {
            current_fun_name: name,
            finished_blocks: vec![],
            n_blocks: 1,
            current_block: vec![],
            vals,
        }
    }

    fn finish(self) -> Fun {
        todo!()
    }

    fn expr(&mut self, expr: Expr) -> InstrIdx {
        let idx = self.current_block.len();
        self.current_block.push(expr);
        InstrIdx(idx as u32)
    }

    fn def_var(&mut self, var: VarId, val: Value) {
        self.vals.insert(var, val);
    }

    fn use_var(&mut self, var: VarId) -> Value {
        // FIXME: clone
        self.vals.get(&var).expect("unbound variable").clone()
    }

    fn create_block(&mut self) -> BlockIdx {
        let block_idx = BlockIdx(self.n_blocks);
        self.n_blocks += 1;
        block_idx
    }
}

pub fn lower_pgm(ctx: &mut Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    let main_name = ctx.fresh_codegen_var(CompilerPhase::ClosureConvert, RepType::Word);
    let mut funs = vec![];
    let mut builder = Builder::new(main_name, &[]);

    lower_expr(ctx, &mut funs, &mut builder, BlockSequel::Return, expr);
    assert!(builder.current_block.is_empty());

    (funs, main_name)
}

fn lower_expr(
    ctx: &mut Ctx, funs: &mut Vec<Fun>, builder: &mut Builder, sequel: BlockSequel, expr: anormal::Expr,
) -> Value {
    match expr {
        anormal::Expr::Unit => builder.expr(Expr::Atom(Atom::Unit)).into(),

        anormal::Expr::Neg(var) => {
            let arg = builder.use_var(var);
            builder.expr(Expr::Neg(arg)).into()
        }

        anormal::Expr::Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            // TODO: No need for a continuation block when the RHS doesn't fork
            let cont_block = builder.create_block();
            let rhs_sequel = BlockSequel::Jump(cont_block);

            let rhs_value = lower_expr(ctx, funs, builder, rhs_sequel, *rhs);
            builder.def_var(id, rhs_value);

            // TODO: Finish current block and switch to cont_block
            lower_expr(ctx, funs, builder, sequel, *body)
        }

        anormal::Expr::LetRec {
            name,
            ty_id,
            args,
            rhs,
            body,
        } => {
            // Generate the function
            let closure_fvs: Vec<VarId> = {
                let mut closure_fvs: FxHashSet<VarId> = Default::default();
                fvs(ctx, &*rhs, &mut closure_fvs);
                closure_fvs.remove(&name);
                for arg in &args {
                    closure_fvs.remove(arg);
                }
                closure_fvs.into_iter().collect()
            };

            let mut args = args;
            let self_arg = ctx.fresh_codegen_var(CompilerPhase::ClosureConvert, RepType::Word);
            args.insert(0, self_arg);

            let mut closure_builder = Builder::new(name, &args);

            let self_val = Value::Arg(0);

            for (fv_idx, fv) in closure_fvs.iter().enumerate() {
                let fv_val = closure_builder.expr(Expr::TupleGet(self_val.clone(), fv_idx));
                closure_builder.def_var(*fv, fv_val.into());
            }

            lower_expr(ctx, funs, &mut closure_builder, BlockSequel::Return, *rhs);
            // TODO: finish the builder and add functions
            let fun_idx: FunIdx = FunIdx(0); // TODO

            // Allocate the closure
            let mut arg_vals = vec![Value::Fun(fun_idx)];
            for fv in closure_fvs {
                arg_vals.push(builder.use_var(fv));
            }

            let closure_val = builder.expr(Expr::Tuple(arg_vals));
            builder.def_var(name, closure_val.into());

            lower_expr(ctx, funs, builder, sequel, *body)
        }

        _ => todo!(),
    }
}

fn fvs(ctx: &Ctx, e: &anormal::Expr, acc: &mut FxHashSet<VarId>) {
    use anormal::Expr::*;
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
        Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            fvs(ctx, rhs, acc);
            fvs(ctx, body, acc);
            acc.remove(id);
        }
        Var(id) => {
            fv(ctx, *id, acc);
        }
        LetRec {
            name,
            ty_id: _,
            args,
            rhs,
            body,
        } => {
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
        Tuple(args) => {
            for arg in args {
                fv(ctx, *arg, acc);
            }
        }
        TupleGet(arg, _) => {
            fv(ctx, *arg, acc);
        }
        ArrayAlloc { len, elem } => {
            fv(ctx, *len, acc);
            fv(ctx, *elem, acc);
        }
        ArrayGet(arg1, arg2) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        ArrayPut(arg1, arg2, arg3) => {
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
