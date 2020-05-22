#![allow(dead_code, unused_imports, unreachable_code)]

use fxhash::{FxHashMap, FxHashSet};
use std::cmp::{max, min};

use super::instr::*;
use super::types::*;

#[derive(Debug, Default)]
pub(crate) struct LiveRangeMap(FxHashMap<VarIdx, LiveRange>);

#[derive(Debug, Default)]
pub(crate) struct LiveRange {
    begin: Option<InstrIdx>,
    end: Option<InstrIdx>,
}

impl LiveRange {
    fn extend_to_use(&mut self, use_site: InstrIdx) {
        self.end = Some(max(self.end.take().unwrap_or(use_site), use_site));
    }

    fn extend_to_def(&mut self, def_site: InstrIdx) {
        assert!(self.begin.is_none() || self.begin == Some(def_site));
        self.begin = Some(def_site);
    }
}

impl LiveRangeMap {
    fn get(&mut self, var: VarIdx) -> &mut LiveRange {
        self.0.entry(var).or_insert(Default::default())
    }

    fn extend_to_use(&mut self, var: VarIdx, use_site: InstrIdx) {
        self.get(var).extend_to_use(use_site)
    }

    fn extend_to_def(&mut self, var: VarIdx, def_site: InstrIdx) {
        self.get(var).extend_to_def(def_site)
    }
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

pub(crate) fn generate_live_ranges(fun: &Fun) -> LiveRangeMap {
    // TODO: update this map as the range is extended in `add_instr_lives` below
    let mut range_map = Default::default();
    let mut instr_live_ins: InstrLiveIns = Default::default();

    let mut changed = true;
    while changed {
        changed = false;

        let mut work_list: Vec<BlockIdx> = fun.exit_blocks().iter().cloned().collect();
        let mut visited: FxHashSet<BlockIdx> = work_list.iter().cloned().collect();

        while let Some(block) = work_list.pop() {
            for instr in fun.block_instrs(block).rev() {
                let mut live_ins = instr_live_ins.remove(instr);
                changed |= add_instr_lives(
                    &mut range_map,
                    &mut live_ins,
                    fun,
                    instr,
                    &mut instr_live_ins,
                );
                instr_live_ins.insert(instr, live_ins);
            }

            for pred in fun.block_preds(block) {
                if !visited.contains(pred) {
                    work_list.push(*pred);
                    visited.insert(*pred);
                }
            }
        }
    }

    range_map
}

fn add_instr_lives(
    range_map: &mut LiveRangeMap, live_ins: &mut LiveInSet, fun: &Fun, instr_idx: InstrIdx,
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
        range_map.extend_to_def(*def, instr_idx);
        live_ins.remove(def);
    }

    // Add uses
    for use_ in instr.uses() {
        range_map.extend_to_use(*use_, instr_idx);
        live_ins.insert(*use_);
    }

    let size_after = live_ins.len();
    size_before != size_after
}
