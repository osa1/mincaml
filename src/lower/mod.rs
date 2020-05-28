#![allow(unused_imports, unused_variables)]

mod block;
mod cfg;
mod ctx;
mod fun;
mod instr;
mod liveness;
mod print;

use block::BlockIdx;
use ctx::Ctx;
use fun::{Fun, FunSig};

use crate::anormal;
use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx as ctx_;
use crate::ctx::VarId;
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

use cfg::CFG;
use instr::*;
pub use print::*;

use cranelift_entity::PrimaryMap;
use std::mem::replace;

// Used when debugging
#[allow(unused_imports)]
use crate::utils;

use fxhash::FxHashSet;

/// Entry point for code generation. Returns generated functions and variable of the main function.
// (TODO: Generate main at call site and pass it here)
pub fn lower_pgm(ctx: &mut ctx_::Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    let mut ctx = Ctx::new(ctx);

    let main_name = ctx.fresh_var(RepType::Word);
    let main_block = ctx.create_block();
    lower_block(&mut ctx, main_block, Sequel::Return, expr);

    (ctx.finish(main_name), main_name)
}

#[derive(Debug, Clone)]
enum Sequel {
    Return,
    // Assign return value to this variable and jump to the label. Used when lowering let bindings.
    Asgn(VarId, BlockIdx),
}

fn finish_block(ctx: &mut Ctx, block: BlockIdx, sequel: Sequel, value: Value) {
    match sequel {
        Sequel::Return => {
            ctx.ret(block, value);
        }
        Sequel::Asgn(lhs, target) => {
            let lhs_val = ctx.use_var(lhs);
            ctx.mov(block, lhs_val, value);
            ctx.jmp(block, target);
        }
    }
}

fn lower_block(ctx: &mut Ctx, block: BlockIdx, sequel: Sequel, expr: anormal::Expr) {
    match expr {
        anormal::Expr::Unit => {
            let val = ctx.iimm(block, 0);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::Int(i) => {
            let val = ctx.iimm(block, i);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::Float(f) => {
            let val = ctx.fimm(block, f);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let val1 = ctx.use_var(arg1);
            let val2 = ctx.use_var(arg2);
            let val;
            match op {
                IntBinOp::Add => {
                    val = ctx.iadd(block, val1, val2);
                }
                IntBinOp::Sub => {
                    val = ctx.isub(block, val1, val2);
                }
            }
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let val1 = ctx.use_var(arg1);
            let val2 = ctx.use_var(arg2);
            let val;
            match op {
                FloatBinOp::Add => {
                    val = ctx.fadd(block, val1, val2);
                }
                FloatBinOp::Sub => {
                    val = ctx.fsub(block, val1, val2);
                }
                FloatBinOp::Mul => {
                    val = ctx.fmul(block, val1, val2);
                }
                FloatBinOp::Div => {
                    val = ctx.fdiv(block, val1, val2);
                }
            }
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::Neg(var) => {
            let val = ctx.use_var(var);
            let val = ctx.neg(block, val);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::FNeg(var) => {
            let val = ctx.use_var(var);
            let val = ctx.fneg(block, val);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::If(var1, var2, cmp, then_e, else_e) => {
            let then_block = ctx.create_block();
            let else_block = ctx.create_block();

            let val1 = ctx.use_var(var1);
            let val2 = ctx.use_var(var2);

            ctx.cond_jmp(block, val1, val2, cmp, then_block, else_block);

            lower_block(ctx, then_block, sequel.clone(), *then_e);
            lower_block(ctx, else_block, sequel, *else_e);
        }

        anormal::Expr::Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            let cont_block = ctx.create_block();
            let rhs_sequel = Sequel::Asgn(id, cont_block);
            lower_block(ctx, block, rhs_sequel, *rhs);
            lower_block(ctx, cont_block, sequel, *body);
        }

        anormal::Expr::Var(var) => {
            let val = ctx.use_var(var);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::LetRec {
            name,
            ty_id,
            mut args,
            rhs,
            body,
        } => {
            // 'name' will refer to the closure tuple. For the actual function create a fresh var
            let fun_var = ctx.fresh_var(RepType::Word);
            // Free variables of the closure will be moved to tuple payload
            // NOTE: An inefficiency here is that if we have deeply nested letrecs we'll be
            // computing fvs of nested letrecs when computing the outer ones. One solution could be
            // to annotate LetRecs with fvs. Doesn't matter in practice though.
            let closure_fvs: Vec<VarId> = {
                let mut closure_fvs: FxHashSet<VarId> = Default::default();
                fvs(ctx, &*rhs, &mut closure_fvs);
                closure_fvs.remove(&name);
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
            ctx.fork_fun(|ctx| {
                // TODO: use def_var
                let entry_block = ctx.create_block();
                let tuple_val = ctx.use_var(name);
                // Bind captured variables in function body
                for (fv_idx, fv) in closure_fvs.iter().enumerate() {
                    let val = ctx.tuple_get(entry_block, tuple_val.clone(), fv_idx);
                    ctx.def_var(*fv, val);
                }
                lower_block(ctx, entry_block, Sequel::Return, *rhs);

                let fun_type = ctx.get_type(ty_id);
                let fun_return_type = match &*fun_type {
                    Type::Fun { ret, .. } => RepType::from(&**ret),
                    _ => panic!("Non-function in function position"),
                };

                FunSig {
                    name: fun_var,
                    args,
                    return_type: fun_return_type,
                }
            });

            // Body
            let mut closure_tuple_args = closure_fvs;
            closure_tuple_args.insert(0, fun_var);
            let tuple_val = ctx.tuple(block, closure_tuple_args.len());
            ctx.def_var(name, tuple_val.clone());
            for (arg_idx, arg) in closure_tuple_args.iter().enumerate() {
                let arg_val = ctx.use_var(*arg);
                ctx.tuple_put(block, tuple_val.clone(), arg_idx, arg_val);
            }

            lower_block(ctx, block, sequel, *body);
        }

        anormal::Expr::App(var, args) => {
            // f(x) -> f.0(f, x)
            let tuple = ctx.use_var(var);
            let fun = ctx.tuple_get(block, tuple, 0);

            let mut arg_vals = Vec::with_capacity(args.len() + 1);
            arg_vals.push(fun.clone());

            for arg in args {
                let arg_val = ctx.use_var(arg);
                arg_vals.push(arg_val);
            }

            let fun_ret_ty = match &*ctx.var_type(var) {
                Type::Fun { args: _, ret } => RepType::from(&**ret),
                other => panic!("Non-function in function position: {:?}", other),
            };

            let call = ctx.call(block, fun, arg_vals, fun_ret_ty);
            finish_block(ctx, block, sequel, call);
        }

        anormal::Expr::Tuple(args) => {
            let tuple = ctx.tuple(block, args.len());
            for (arg_idx, arg) in args.iter().enumerate() {
                let arg_val = ctx.use_var(*arg);
                ctx.tuple_put(block, tuple.clone(), arg_idx, arg_val);
            }
            finish_block(ctx, block, sequel, tuple);
        }

        anormal::Expr::TupleGet(tuple, idx) => {
            let tuple = ctx.use_var(tuple);
            // TODO: not possible to generate instructions for this without knowing the type
            let ret = ctx.tuple_get(block, tuple, idx);
            finish_block(ctx, block, sequel, ret);
        }

        anormal::Expr::ArrayAlloc { len, elem } => {
            let len = ctx.use_var(len);
            let array = ctx.array_alloc(block, len.clone());
            let elem = ctx.use_var(elem);

            let idx = ctx.iimm(block, 0);

            let loop_cond_block = ctx.create_block();
            ctx.jmp(block, loop_cond_block);

            let loop_body_block = ctx.create_block();
            let cont_block = ctx.create_block();

            // loop_cond
            ctx.cond_jmp(
                loop_cond_block,
                idx.clone(),
                len,
                Cmp::Equal,
                cont_block,
                loop_body_block,
            );

            // loop_body
            ctx.array_put(loop_body_block, array.clone(), idx.clone(), elem);
            let inc = ctx.iimm(loop_body_block, 1);
            let idx_plus_one = ctx.iadd(loop_body_block, idx.clone(), inc);
            ctx.mov(loop_body_block, idx, idx_plus_one);
            ctx.jmp(loop_body_block, loop_cond_block);

            finish_block(ctx, block, sequel, array);
        }

        anormal::Expr::ArrayGet(array, idx) => {
            // TODO: need the type here
            let array = ctx.use_var(array);
            let idx = ctx.use_var(idx);
            let ret = ctx.array_get(block, array, idx);
            finish_block(ctx, block, sequel, ret);
        }

        anormal::Expr::ArrayPut(array, idx, val) => {
            // TODO need the type here
            let array = ctx.use_var(array);
            let idx = ctx.use_var(idx);
            let val = ctx.use_var(val);
            let ret = ctx.array_put(block, array, idx, val);
            finish_block(ctx, block, sequel, ret);
        }
    }
}

/*
// A block currently being built
struct BlockBuilder {
    idx: BlockIdx,
    stmts: Vec<Stmt>,
    comment: Option<String>,
}

impl BlockBuilder {
    fn new(idx: BlockIdx) -> Self {
        Self {
            idx,
            stmts: vec![],
            comment: None,
        }
    }

    fn instr(&mut self, ctx: &mut CcCtx, kind: InstrKind) -> InstrIdx {
        let idx = ctx.instrs.next_key();
        if idx.as_u32() == 0 {
            // First instruction
            let node = InstrNode {
                idx,
                next: idx,
                prev: idx,
                kind,
            };
            ctx.instrs.push(node);
        } else {
            let node = InstrNode {
                idx,
                next: idx,
                prev: ctx.last_instr,
                kind,
            };
            ctx.instrs.push(node);
        }

        ctx.last_instr = idx;
        idx
    }

    fn asgn(&mut self, ctx: &mut CcCtx, lhs: VarId, rhs: Expr) -> InstrIdx {
        self.stmts.push(Stmt::Asgn(Asgn { lhs, rhs }));
        ctx.last_instr // TODO
    }

    fn expr(&mut self, ctx: &mut CcCtx, expr: Expr) -> InstrIdx {
        self.stmts.push(Stmt::Expr(expr));
        ctx.last_instr // TODO
    }
}

// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    // Functions generated so far
    funs: Vec<Fun>,
    // Blocks generated so far for the current function
    blocks: PrimaryMap<BlockIdx, BlockData>,
    // Control-flow graph of the current function
    cfg: CFG,
    // Instructions of the current function
    last_instr: InstrIdx,
    instrs: InstrMap,
}

impl<'ctx> CcCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx) -> Self {
        Self {
            ctx,
            funs: vec![],
            blocks: PrimaryMap::new(),
            cfg: CFG::new(),
            last_instr: InstrIdx::from_u32(0),
            instrs: PrimaryMap::new(),
        }
    }

    fn fresh_var(&mut self, rep_type: RepType) -> VarId {
        self.ctx.fresh_codegen_var(ClosureConvert, rep_type)
    }

    fn create_block(&mut self) -> BlockBuilder {
        let idx = self.blocks.push(BlockData::NA);
        BlockBuilder::new(idx)
    }

    fn fork_fun<F: FnOnce(&mut CcCtx) -> FunSig>(&mut self, fork: F) {
        let blocks = replace(&mut self.blocks, PrimaryMap::new());
        let cfg = replace(&mut self.cfg, CFG::new());
        let FunSig {
            name,
            args,
            return_type,
        } = fork(self);
        let fun_blocks = replace(&mut self.blocks, blocks);
        let cfg = replace(&mut self.cfg, cfg);
        self.funs.push(Fun {
            name,
            args,
            blocks: fun_blocks,
            cfg,
            return_type,
        });
    }

    fn finish_block(&mut self, block: BlockBuilder, sequel: Sequel, value: VarId) {
        let BlockBuilder {
            idx,
            mut stmts,
            comment,
        } = block;

        let exit = match sequel {
            Sequel::Return => Exit::Return(value),
            Sequel::Asgn(lhs, target) => {
                // TODO: Should we handle this case in the call site? Or make it impossible to
                // happen somehow?
                if lhs != value {
                    stmts.push(Stmt::Asgn(Asgn {
                        lhs,
                        rhs: Expr::Atom(Atom::Var(value)),
                    }));
                }
                self.cfg.add_successor(idx, target);
                Exit::Jump(target)
            }
        };

        let block = Block {
            idx,
            comment,
            stmts,
            exit,
        };

        self.finish_block_(block);
    }

    fn finish_block_(&mut self, block: Block) {
        let idx = block.idx;
        assert!(self.blocks[idx].is_NA());
        self.blocks[idx] = BlockData::Block(block);
    }
}

pub fn lower_pgm(ctx: &mut Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    let mut ctx = CcCtx::new(ctx);

    let main_name = ctx.fresh_var(RepType::Word);
    let main_block = ctx.create_block();
    cc_block(&mut ctx, main_block, Sequel::Return, expr);

    ctx.funs.push(Fun {
        name: main_name,
        args: vec![],
        blocks: ctx.blocks,
        cfg: ctx.cfg,
        return_type: RepType::Word,
    });

    (ctx.funs, main_name)
}

fn bind_atom(ctx: &mut CcCtx, block: &mut BlockBuilder, atom: Atom) -> VarId {
    match atom {
        Atom::Unit => {
            let tmp = ctx.fresh_var(RepType::Word);
            block.asgn(ctx, tmp, Expr::Atom(Atom::Unit));
            tmp
        }
        Atom::Int(i) => {
            let tmp = ctx.fresh_var(RepType::Word);
            block.asgn(ctx, tmp, Expr::Atom(Atom::Int(i)));
            tmp
        }
        Atom::Float(f) => {
            let tmp = ctx.fresh_var(RepType::Float);
            block.asgn(ctx, tmp, Expr::Atom(Atom::Float(f)));
            tmp
        }
        Atom::Var(var) => var,
    }
}

// Returns whether the added block was a fork (i.e. then or else branch of an if)
fn cc_block(ctx: &mut CcCtx, mut block: BlockBuilder, sequel: Sequel, expr: anormal::Expr) {
    match expr {
        anormal::Expr::Unit => {
            let tmp = bind_atom(ctx, &mut block, Atom::Unit);
            ctx.finish_block(block, sequel, tmp)
        }

        anormal::Expr::Int(i) => {
            let tmp = bind_atom(ctx, &mut block, Atom::Int(i));
            ctx.finish_block(block, sequel, tmp)
        }

        anormal::Expr::Float(f) => {
            let tmp = bind_atom(ctx, &mut block, Atom::Float(f));
            ctx.finish_block(block, sequel, tmp)
        }

        anormal::Expr::Neg(var) => {
            let tmp = ctx.fresh_var(RepType::Word);
            block.asgn(ctx, tmp, Expr::Neg(var));
            ctx.finish_block(block, sequel, tmp);
        }

        anormal::Expr::FNeg(var) => {
            let tmp = ctx.fresh_var(RepType::Float);
            block.asgn(ctx, tmp, Expr::FNeg(var));
            ctx.finish_block(block, sequel, tmp);
        }

        anormal::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx, RepType::Word);
            block.asgn(ctx, tmp, Expr::IBinOp(BinOp { op, arg1, arg2 }));
            ctx.finish_block(block, sequel, tmp);
        }

        anormal::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx, RepType::Float);
            block.asgn(ctx, tmp, Expr::FBinOp(BinOp { op, arg1, arg2 }));
            ctx.finish_block(block, sequel, tmp);
        }

        anormal::Expr::If(v1, v2, cmp, e1, e2) => {
            let then_block = ctx.create_block();
            let else_block = ctx.create_block();
            ctx.finish_block_(Block {
                idx: block.idx,
                comment: block.comment,
                stmts: block.stmts,
                exit: Exit::Branch {
                    v1,
                    v2,
                    cond: cmp,
                    then_block: then_block.idx,
                    else_block: else_block.idx,
                },
            });
            cc_block(ctx, then_block, sequel.clone(), *e1);
            cc_block(ctx, else_block, sequel, *e2);
        }

        anormal::Expr::Var(var) => {
            ctx.finish_block(block, sequel, var);
        }

        anormal::Expr::Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            // TODO: When the RHS is not if-then-else we can continue extending the last block RHS
            // generates and avoid creating a block for the continuation.
            let cont_block = ctx.create_block();
            let rhs_sequel = Sequel::Asgn(id, cont_block.idx);
            cc_block(ctx, block, rhs_sequel, *rhs);
            cc_block(ctx, cont_block, sequel, *body)
        }

        anormal::Expr::LetRec {
            name,
            ty_id,
            mut args,
            rhs,
            body,
        } => {
            // TODO: Not sure about reusing 'name' in multiple places below.

            // After cc 'name' will refer to the closure tuple. For the function we'll need a fresh
            // variable.
            let fun_var = ctx.fresh_var(RepType::Word);

            // Free variables of the closure will be moved to tuple payload
            // NOTE: An inefficiency here is that if we have deeply nested letrecs we'll be
            // computing fvs of nested letrecs when computing the outer ones. One solution could be
            // to annotate LetRecs with fvs. Doesn't matter in practice though.
            let closure_fvs: Vec<VarId> = {
                let mut closure_fvs: FxHashSet<VarId> = Default::default();
                fvs(ctx.ctx, &*rhs, &mut closure_fvs);
                closure_fvs.remove(&name);
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
            ctx.fork_fun(|ctx| {
                let mut entry_block = ctx.create_block();
                // Bind captured variables in function body
                for (fv_idx, fv) in closure_fvs.iter().enumerate() {
                    entry_block.asgn(ctx, *fv, Expr::TupleGet(name, fv_idx + 1));
                }
                cc_block(ctx, entry_block, Sequel::Return, *rhs);

                let fun_type = ctx.ctx.get_type(ty_id);
                let fun_return_type = match &*fun_type {
                    Type::Fun { ret, .. } => RepType::from(&**ret),
                    _ => panic!("Non-function in function position"),
                };

                FunSig {
                    name: fun_var,
                    args,
                    return_type: fun_return_type,
                }
            });

            // Body
            let mut closure_tuple_args = closure_fvs;
            closure_tuple_args.insert(0, fun_var);
            block.asgn(
                ctx,
                name,
                Expr::Tuple {
                    len: closure_tuple_args.len(),
                },
            );
            for (arg_idx, arg) in closure_tuple_args.iter().enumerate() {
                block.expr(ctx, Expr::TuplePut(name, arg_idx, *arg));
            }
            cc_block(ctx, block, sequel, *body)
        }

        anormal::Expr::App(fun, mut args) => {
            // f(x) -> f.0(f, x)
            let fun_tmp = ctx.fresh_var(RepType::Word);
            block.asgn(ctx, fun_tmp, Expr::TupleGet(fun, 0));
            args.insert(0, fun);

            let fun_ret_ty = match &*ctx.ctx.var_type(fun) {
                Type::Fun { args: _, ret } => RepType::from(&**ret),
                other => panic!("Non-function in function position: {:?}", other),
            };
            let ret_tmp = sequel.get_ret_var(ctx, fun_ret_ty);

            block.asgn(ctx, ret_tmp, Expr::App(fun_tmp, args, fun_ret_ty));
            ctx.finish_block(block, sequel, ret_tmp);
        }

        anormal::Expr::Tuple(args) => {
            let ret_tmp = sequel.get_ret_var(ctx, RepType::Word);
            block.asgn(ctx, ret_tmp, Expr::Tuple { len: args.len() });
            for (arg_idx, arg) in args.iter().enumerate() {
                block.expr(ctx, Expr::TuplePut(ret_tmp, arg_idx, *arg));
            }
            ctx.finish_block(block, sequel, ret_tmp);
        }

        anormal::Expr::TupleGet(tuple, idx) => {
            let elem_ty = match &*ctx.ctx.var_type(tuple) {
                Type::Tuple(args) => RepType::from(&args[idx]),
                other => panic!(
                    "Non-tuple type in tuple position: {:?} (type={:?})",
                    tuple, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            block.asgn(ctx, ret_tmp, Expr::TupleGet(tuple, idx));
            ctx.finish_block(block, sequel, ret_tmp);
        }

        anormal::Expr::ArrayAlloc { len, elem } => {
            let array_tmp = sequel.get_ret_var(ctx, RepType::Word);
            block.asgn(ctx, array_tmp, Expr::ArrayAlloc { len });

            let idx_var = ctx.fresh_var(RepType::Word);
            block.asgn(ctx, idx_var, Expr::Atom(Atom::Int(0)));

            let loop_cond_block = ctx.create_block();
            let mut loop_body_block = ctx.create_block();
            let cont_block = ctx.create_block();

            ctx.finish_block_(Block {
                idx: block.idx,
                comment: block.comment,
                stmts: block.stmts,
                exit: Exit::Jump(loop_cond_block.idx),
            });

            // loop_cond
            ctx.finish_block_(Block {
                idx: loop_cond_block.idx,
                comment: Some("array loop cond".to_string()),
                stmts: vec![],
                exit: Exit::Branch {
                    v1: idx_var,
                    v2: len,
                    cond: Cmp::Equal,
                    then_block: cont_block.idx,
                    else_block: loop_body_block.idx,
                },
            });

            // loop_body
            let idx_inc_var = ctx.fresh_var(RepType::Word);
            loop_body_block.expr(ctx, Expr::ArrayPut(array_tmp, idx_var, elem));
            loop_body_block.asgn(ctx, idx_inc_var, Expr::Atom(Atom::Int(1)));
            loop_body_block.asgn(
                ctx,
                idx_var,
                Expr::IBinOp(BinOp {
                    op: IntBinOp::Add,
                    arg1: idx_var,
                    arg2: idx_inc_var,
                }),
            );
            ctx.finish_block_(Block {
                idx: loop_body_block.idx,
                comment: Some("array body".to_string()),
                stmts: loop_body_block.stmts,
                exit: Exit::Jump(loop_cond_block.idx),
            });

            ctx.finish_block(cont_block, sequel, array_tmp);
        }

        anormal::Expr::ArrayGet(array, idx) => {
            let elem_ty = match &*ctx.ctx.var_type(array) {
                Type::Array(elem_ty) => RepType::from(&**elem_ty),
                other => panic!(
                    "Non-array type in array position: {:?} (type={:?})",
                    array, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            block.asgn(ctx, ret_tmp, Expr::ArrayGet(array, idx));
            ctx.finish_block(block, sequel, ret_tmp);
        }

        anormal::Expr::ArrayPut(array, idx, val) => {
            let elem_ty = match &*ctx.ctx.var_type(array) {
                Type::Array(elem_ty) => RepType::from(&**elem_ty),
                other => panic!(
                    "Non-array type in array position: {:?} (type={:?})",
                    array, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            block.asgn(ctx, ret_tmp, Expr::ArrayPut(array, idx, val));
            ctx.finish_block(block, sequel, ret_tmp);
        }
    }
}
*/

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
