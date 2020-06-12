// Two algorithms for liveness analysis on SSA:
//
// - Modern Compiler Implementation in Java, Second Edition, section 19.6
//   by Andrew Appel
//
// - Linear Scan Register Allocation on SSA Form by Wimmer et al.
//
// We use the first one to generate live-ins, and second one to build live intervals.

use super::block::BlockIdx;
use super::fun::Fun;
use super::instr::{InstrIdx, Value, ValueIdx};
use crate::ctx::Ctx;

use cranelift_entity::SecondaryMap;
use fxhash::FxHashSet;

use std::fmt;
use std::mem::replace;

#[derive(Debug)]
pub struct LiveRange {
    begin: InstrIdx,
    end: InstrIdx,
}

// Non-overlapping list of live ranges
#[derive(Debug)]
pub struct LiveInterval(Vec<LiveRange>);

#[derive(Debug)]
pub struct Liveness {
    /// Live-ins at instructions
    live_ins: SecondaryMap<InstrIdx, FxHashSet<ValueIdx>>,

    /// Live-outs at instructions
    live_outs: SecondaryMap<InstrIdx, FxHashSet<ValueIdx>>,
}

impl Liveness {
    pub fn instr_live_ins(&self, instr_idx: InstrIdx) -> &FxHashSet<ValueIdx> {
        &self.live_ins[instr_idx]
    }

    pub fn instr_live_outs(&self, instr_idx: InstrIdx) -> &FxHashSet<ValueIdx> {
        &self.live_outs[instr_idx]
    }
}

// BuildIntervals in paper
//
// Input blocks should be ordered so that
//
// - All dominators of a block come before the block
// - All blocks belonging to the same loop are contiguous
//
pub fn build_intervals(fun: &Fun, liveness: &Liveness) {

}

// We implement simple dataflow-based liveness analysis for now.
pub fn gen_liveness(fun: &Fun) -> Liveness {
    let mut liveness = Liveness {
        live_ins: SecondaryMap::new(),
        live_outs: SecondaryMap::new(),
    };

    loop {
        let mut work_list = fun.exit_blocks.clone();
        let mut updated = false;
        let mut visited: FxHashSet<BlockIdx> = Default::default();

        while let Some(block) = work_list.pop() {
            updated |= update_block_liveness(fun, &mut visited, &mut liveness, block);

            for pred in &fun.preds[block] {
                if !visited.contains(pred) {
                    work_list.push(*pred);
                }
            }
        }

        if !updated {
            break;
        }
    }

    liveness
}

fn update_block_liveness(
    fun: &Fun, visited: &mut FxHashSet<BlockIdx>, liveness: &mut Liveness, block_idx: BlockIdx,
) -> bool {
    visited.insert(block_idx);

    let mut updated = false;

    let block = &fun.blocks[block_idx];
    let first_instr_idx = block.first_instr;
    let last_instr_idx = block.last_instr;

    // Add phi operands as live-outs to predecessors
    for phi_idx in &fun.block_phis[block_idx] {
        let phi = &fun.phis[*phi_idx];
        for (pred_idx, pred_val_idx) in phi.values.iter().enumerate() {
            let pred_block_idx = fun.preds[block_idx][pred_idx];
            let pred_block_last_instr = fun.blocks[pred_block_idx].last_instr;
            updated |= liveness.live_outs[pred_block_last_instr].insert(*pred_val_idx);
        }
    }

    let mut instr_idx = last_instr_idx;
    loop {
        let instr = &fun.instrs[instr_idx];

        // Update live-outs
        if instr_idx == last_instr_idx {
            for succ in &fun.succs[block_idx] {
                let succ_instr = fun.blocks[*succ].first_instr;
                for succ_live_in in &liveness.live_ins[succ_instr] {
                    updated |= liveness.live_outs[instr_idx].insert(*succ_live_in);
                }
            }
        } else {
            let succ_instr = instr.next;
            for succ_live_in in &liveness.live_ins[succ_instr] {
                updated |= liveness.live_outs[instr_idx].insert(*succ_live_in);
            }
        }

        // Update live-ins: add uses
        for use_ in instr.kind.uses() {
            updated |= liveness.live_ins[instr_idx].insert(use_);
        }

        // Update live-ins: add live-ins of successors that are not defined in this instruction
        for out in &liveness.live_outs[instr_idx] {
            if fun.values[*out] != Value::Instr(instr_idx) {
                updated |= liveness.live_ins[instr_idx].insert(*out);
            }
        }

        if instr_idx == first_instr_idx {
            break;
        }

        instr_idx = instr.prev;
    }

    updated
}

//
// Debug interface
//

impl Liveness {
    pub fn debug<'a>(&'a self, ctx: &'a Ctx, fun: &'a Fun) -> LivenessDebug<'a> {
        LivenessDebug {
            ctx,
            fun,
            liveness: self,
        }
    }
}

// Provides a better `Debug` impl for `Liveness`: shows actual values instead of indices
pub struct LivenessDebug<'a> {
    ctx: &'a Ctx,
    fun: &'a Fun,
    liveness: &'a Liveness,
}

pub fn value_set_debug<'a>(
    set: &'a FxHashSet<ValueIdx>, ctx: &'a Ctx, fun: &'a Fun,
) -> ValueSetDebug<'a> {
    ValueSetDebug { set, ctx, fun }
}

// Provides a better `Debug` impl for `FxHashSet<ValueIdx>`: shows actual values instead of indices
pub struct ValueSetDebug<'a> {
    set: &'a FxHashSet<ValueIdx>,
    ctx: &'a Ctx,
    fun: &'a Fun,
}

// Provides a better `Debug` impl for `ValueIdx`: shows the value instead of index
struct ValueIdxDebug<'a> {
    value: ValueIdx,
    ctx: &'a Ctx,
    fun: &'a Fun,
}

impl<'a> fmt::Debug for ValueIdxDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = &self.fun.values[self.value];
        value.debug(self.ctx).fmt(f)
    }
}

impl<'a> fmt::Debug for ValueSetDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut set = f.debug_set();
        for value in self.set {
            set.entry(&ValueIdxDebug {
                value: *value,
                ctx: self.ctx,
                fun: self.fun,
            });
        }
        set.finish()
    }
}

impl<'a> fmt::Debug for LivenessDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("live-ins:  ")?;
        let mut map = f.debug_map();
        for (idx, live_in) in self.liveness.live_ins.values().enumerate() {
            map.entry(
                &idx,
                &ValueSetDebug {
                    set: live_in,
                    ctx: self.ctx,
                    fun: self.fun,
                },
            );
        }
        map.finish()?;

        f.write_str("\nlive-outs: ")?;
        let mut map = f.debug_map();
        for (idx, live_out) in self.liveness.live_outs.values().enumerate() {
            map.entry(
                &idx,
                &ValueSetDebug {
                    set: live_out,
                    ctx: self.ctx,
                    fun: self.fun,
                },
            );
        }
        map.finish()
    }
}
