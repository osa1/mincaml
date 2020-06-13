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

//
// Generating live ranges and intervals
//

#[derive(Debug, Clone)]
pub struct LiveRange {
    begin: InstrIdx,
    end: InstrIdx,
}

// Non-overlapping list of live ranges
type LiveInterval = Vec<LiveRange>;

// TODO: We could implement the algorithm in `build_intervals` more efficiently if we had def sites
// in `Fun`.

// TODO: The algorithm below makes some assumptions that currently doesn't hold:
//
// - Instructions of blocks are numbered consecutively, so for example if a live range is (5, 10)
//   then instructions 5, 6, .. 10 all belong to the same block. (I think this currently holds)
//
// - What else?

#[derive(Debug)]
enum Work {
    // Value is live-out at the block. There are two cases:
    //
    // - The value is defined in this block: we'll have a live range from the defining instruction
    //   to the end of the block.
    //
    // - The value is NOT defined in this block: the value should be live-out in predecessors of
    //   this block.
    //
    // (NB. In the first case the value may be defined as phi. I don't think that's a special case
    // though, a phi definition is definition like any other for the purposes of interval analysis)
    LiveOutAtBlock {
        block_idx: BlockIdx,
        value: ValueIdx,
    },
    // Value is live-in at the instruction. Similar to `LiveOutAtBlock`, there are two cases, and
    // they're handled the same.
    LiveInAtInstr {
        instr_idx: InstrIdx,
        value: ValueIdx,
    },
    // Value is live-out at the instruction
    // LiveOutAtInstr(InstrIdx, ValueIdx),
}

// Algorithm from "Modern Compiler Impl in ...", section 19.6
pub fn build_intervals(ctx: &Ctx, fun: &Fun) -> SecondaryMap<ValueIdx, LiveInterval> {
    let mut intervals: SecondaryMap<ValueIdx, LiveInterval> = SecondaryMap::new();

    // for each variable v
    for (value_idx, uses) in fun.value_use_sites.iter() {
        println!(
            "{:?} use sites: {:?}",
            value_idx.debug(ctx, fun),
            uses.iter()
                // .map(|use_idx| (use_idx, use_idx.debug(ctx, fun)))
                .map(|use_idx| use_idx.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        // Only consider instructions and phis
        match fun.values[value_idx] {
            Value::Global(_) | Value::Arg(_) => {
                continue;
            }
            Value::Instr(_) | Value::Phi(_) => {}
        }

        let mut work_list: Vec<Work> = vec![];

        for use_idx in uses {
            let user = &fun.values[*use_idx];
            match user {
                Value::Global(_) => panic!(
                    "Variable {:?} used by global: {:?}",
                    value_idx.debug(ctx, fun),
                    user.debug(ctx)
                ),
                Value::Arg(arg) => panic!(
                    "Variable {:?} used by function argument {}",
                    value_idx.debug(ctx, fun),
                    arg
                ),
                Value::Instr(instr_idx) => {
                    work_list.push(Work::LiveInAtInstr {
                        instr_idx: *instr_idx,
                        value: value_idx,
                    });
                }
                Value::Phi(phi_idx) => {
                    // Value used by a phi: if it's Nth argument of the phi then it should be
                    // live-out at the Nth predecessor of phi's block
                    let phi = &fun.phis[*phi_idx];
                    // Index of the value in phi
                    let mut phi_value_idx = None;
                    for (phi_op_idx, phi_op) in phi.values.iter().enumerate() {
                        if *phi_op == value_idx {
                            phi_value_idx = Some(phi_op_idx);
                            break;
                        }
                    }

                    let phi_value_idx = phi_value_idx.unwrap();
                    let phi_block = phi.owner;
                    let pred_block = fun.preds[phi_block][phi_value_idx];
                    work_list.push(Work::LiveOutAtBlock {
                        block_idx: pred_block,
                        value: value_idx,
                    });
                }
            }
        }

        let mut visited: FxHashSet<BlockIdx> = Default::default();
        while let Some(work_) = work_list.pop() {
            match work_ {
                Work::LiveOutAtBlock { block_idx, value } => {
                    visited.insert(block_idx);

                    // Find the def site of `value` in the block. If not defined in the block then
                    // it should be live-out at predecessors.

                    let block = &fun.blocks[block_idx];
                    match fun.values[value] {
                        Value::Global(_) | Value::Arg(_) => {
                            panic!();
                        }
                        Value::Instr(value_instr_idx) => {
                            // Is the instructions defined at this block?
                            let mut instr_idx = block.last_instr;
                            loop {
                                if instr_idx == value_instr_idx {
                                    // Found the def site
                                    intervals[value].push(LiveRange {
                                        begin: instr_idx,
                                        end: block.last_instr,
                                    });
                                    break;
                                }

                                if instr_idx == block.first_instr {
                                    // Reached the beginning of the block: value is not defined
                                    // here. Continue with predecessors.
                                    intervals[value].push(LiveRange {
                                        begin: block.first_instr,
                                        end: block.last_instr,
                                    });
                                    for pred in &fun.preds[block_idx] {
                                        if !visited.contains(pred) {
                                            work_list.push(Work::LiveOutAtBlock {
                                                block_idx: *pred,
                                                value,
                                            });
                                        }
                                    }
                                    break;
                                }

                                instr_idx = fun.instrs[instr_idx].prev;
                            }
                        }
                        Value::Phi(phi_idx) => {
                            // Is the phi defined in this block?
                            let phi = &fun.phis[phi_idx];
                            if phi.owner == block_idx {
                                // Phi defined in this block
                                intervals[value].push(LiveRange {
                                    begin: block.first_instr,
                                    end: block.last_instr,
                                });
                            } else {
                                // Phi not defined at this block, it should be live-out in
                                // predecessors
                                intervals[value].push(LiveRange {
                                    begin: block.first_instr,
                                    end: block.last_instr,
                                });
                                for pred in &fun.preds[block_idx] {
                                    if !visited.contains(pred) {
                                        work_list.push(Work::LiveOutAtBlock {
                                            block_idx: *pred,
                                            value,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                Work::LiveInAtInstr {
                    instr_idx: live_in_instr_idx,
                    value,
                } => {
                    let instr = &fun.instrs[live_in_instr_idx];
                    let block = &fun.blocks[instr.block];

                    match fun.values[value] {
                        Value::Global(_) | Value::Arg(_) => {
                            panic!();
                        }

                        Value::Instr(value_instr_idx) => {
                            // Is the instructions defined at this block?
                            let mut instr_idx = live_in_instr_idx;
                            loop {
                                if instr_idx == value_instr_idx {
                                    // Found the def site
                                    intervals[value].push(LiveRange {
                                        begin: instr_idx,
                                        end: live_in_instr_idx,
                                    });
                                    break;
                                }

                                if instr_idx == block.first_instr {
                                    // Reached the beginning of the block: value is not defined
                                    // here. Continue with predecessors.
                                    intervals[value].push(LiveRange {
                                        begin: block.first_instr,
                                        end: block.last_instr,
                                    });
                                    for pred in &fun.preds[block.idx] {
                                        if !visited.contains(pred) {
                                            work_list.push(Work::LiveOutAtBlock {
                                                block_idx: *pred,
                                                value,
                                            });
                                        }
                                    }
                                    break;
                                }

                                instr_idx = fun.instrs[instr_idx].prev;
                            }
                        }
                        Value::Phi(phi_idx) => {
                            // Is the phi defined in this block?
                            let phi = &fun.phis[phi_idx];
                            if phi.owner == block.idx {
                                // Phi defined in this block
                                intervals[value].push(LiveRange {
                                    begin: block.first_instr,
                                    end: block.last_instr,
                                });
                            } else {
                                // Phi not defined at this block, it should be live-out in
                                // predecessors
                                intervals[value].push(LiveRange {
                                    begin: block.first_instr,
                                    end: block.last_instr,
                                });
                                for pred in &fun.preds[block.idx] {
                                    if !visited.contains(pred) {
                                        work_list.push(Work::LiveOutAtBlock {
                                            block_idx: *pred,
                                            value,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    intervals
}

//
// Generating live-ins and live-outs
//

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

impl<'a> fmt::Debug for ValueSetDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut set = f.debug_set();
        for value in self.set {
            set.entry(&value.debug(self.ctx, self.fun));
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
