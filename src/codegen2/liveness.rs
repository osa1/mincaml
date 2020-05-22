#![allow(dead_code, unused_imports, unreachable_code)]

use fxhash::{FxHashMap, FxHashSet};

use super::instr::*;
use super::types::*;

// Maps variables to instructions where they're used (aka. they're a live-in).
pub(crate) struct LiveRangeMap(FxHashMap<VarIdx, LiveRange>);

// TODO: Not sure how to represent this best. Example:
//
// block1 (def x)
//   -> block2         -> block4
//   -> block3 (use x) -> block4
//
// Here we don't want to keep x alive in block2. If we use (begin, end) the range will be (1, 3),
// but that means in block 2 we won't be using register for x.
// 
// A more precise implementation would be a set of BlockIdxs: {1, 3}. We'll need min and max
// methods on this set for linear scan register allocation.
//
pub(crate) struct LiveRange {
    begin: InstrIdx,
    end: InstrIdx,
}

// Live variables at an instruction =
//     variables used + (variables live in successors - variables defined)
//
// The "variables live in successors" part requires mapping instructions to their live-ins.
// Currently use a hash map for that.
//
// Because live vars of an instruction depends on live vars of its successors, traverse the CFG in
// reverse order, from exit node to entry node, following predecessors.

type LiveInSet = FxHashSet<VarIdx>;

#[derive(Debug, Default)]
struct InstrLiveIns(FxHashMap<InstrIdx, LiveInSet>);

impl InstrLiveIns {
    fn get(&mut self, instr: InstrIdx) -> &mut LiveInSet {
        self.0.entry(instr).or_insert_with(|| Default::default())
    }

    fn remove(&mut self, instr: InstrIdx) -> LiveInSet {
        self.0.remove(&instr).unwrap_or_else(|| Default::default())
    }

    fn insert(&mut self, instr: InstrIdx, live_ins: LiveInSet) {
        self.0.insert(instr, live_ins);
    }
}

pub(crate) fn generate_live_ranges(fun: &FunctionBuilder) -> LiveRangeMap {
    // TODO: update this map as the range is extended in `add_instr_lives` below
    let mut range_map = Default::default();
    let mut instr_live_ins: InstrLiveIns = Default::default();

    let exit_block = fun.exit_block();

    let mut changed = true;
    while changed {
        changed = false;

        let mut work_list = vec![exit_block];
        let mut visited: FxHashSet<BlockIdx> = Default::default();
        visited.insert(exit_block);

        while let Some(block) = work_list.pop() {
            for instr in fun.block_instrs(block).rev() {
                let mut live_ins = instr_live_ins.remove(instr);
                changed |= add_instr_lives(&mut live_ins, fun, instr, &mut instr_live_ins);
                instr_live_ins.insert(instr, live_ins);
            }

            for pred in fun.block_preds(block) {
                if !visited.contains(pred) {
                    work_list.push(*pred);
                }
            }
        }
    }

    LiveRangeMap(range_map)
}

fn add_instr_lives(
    live_ins: &mut LiveInSet, fun: &FunctionBuilder, instr_idx: InstrIdx,
    instr_live_ins: &mut InstrLiveIns,
) -> bool {
    let instr = fun.instr(instr_idx);

    let size_before = live_ins.len();

    // Add live_ins of successors.
    for succ_block in instr.jump_targets() {
        let block_entry = fun.block_entry(*succ_block);
        let block_live_ins = instr_live_ins.get(block_entry);
        // We get an empty set here for self loops, which makes sense.
        for live_in in block_live_ins.iter() {
            live_ins.insert(*live_in);
        }
    }

    // Remove defs
    for def in instr.defs() {
        live_ins.remove(def);
    }

    // Add uses
    for use_ in instr.uses() {
        live_ins.insert(*use_);
    }

    let size_after = live_ins.len();
    size_before != size_after
}
