// Implements "Optimized Interval Splitting in a Linear Scan Register Allocator" my Wimmer et al.

// NOTE: We create new instructions (mov) when splitting intervals below, and `InstrIdx` of
// those instructions do not follow the convention that all instructions in a block that start with
// i1 and end with i2 are in range (i1 .. i2). This shouldn't break anything as the instructions we
// generate don't affect register allocation for remaining intervals, but it's still something to
// keep in mind for the passes that follow.

// TODO: Make is easier to keep range lists in sorted order, and searching in them.

use super::fun::Fun;
use super::instr::{InstrIdx, Value, ValueIdx};
use super::liveness::{LiveInterval, LiveIntervalMap, LiveRange};
use crate::{ctx::Ctx, utils::NoAlternate};

use std::cmp::{min, Reverse};
use std::mem::replace;

#[rustfmt::skip]
#[derive(Debug, Clone, Copy)]
pub enum Reg {
    RAX = 0, RCX, RDX, RBX, RSI, RDI, RSP, RBP,
    R8, R9, R10, R11, R12, R13, R14, R15,
}

impl From<usize> for Reg {
    fn from(i: usize) -> Self {
        use Reg::*;
        match i {
            0 => RAX,
            1 => RCX,
            2 => RDX,
            3 => RBX,
            4 => RSI,
            5 => RDI,
            6 => RSP,
            7 => RBP,
            8 => R8,
            9 => R9,
            10 => R10,
            11 => R11,
            12 => R12,
            13 => R13,
            14 => R14,
            15 => R15,
            _ => panic!("Invalid reg idx: {}", i),
        }
    }
}

#[derive(Debug)]
pub struct Allocation {
    value: ValueIdx,
    // 'ranges' is sorted in reverse order of 'begin', so the last range in the range starts first.
    // We pop assigned ranges from this vector as we split the value into multiple assignments.
    ranges: Vec<LiveRange>,
    reg: Reg,
}

pub fn reg_alloc(
    mut live_intervals: LiveIntervalMap, ctx: &Ctx, fun: &Fun,
) -> Vec<(ValueIdx, Reg)> {
    let mut assigns = vec![];

    // Ranges sorted by decreasing start positions so that pop gives us the one that starts next
    let mut unhandled: Vec<(ValueIdx, Vec<LiveRange>)> = vec![];

    // TODO: No IntoIterator for SecondaryMap??
    for (value_idx, interval) in live_intervals.map.iter_mut() {
        let ranges = &interval.ranges;
        if ranges.is_empty() {
            continue;
        }

        // Using `replace` below as we can't consume the vecs (SecondaryMap doesn't implelement
        // IntoIterator)
        let mut ranges = replace(&mut interval.ranges, vec![]);
        ranges.reverse();

        // If this returns `Ok(idx)` then there's another value here with the same begin position,
        // doesn't matter which one comes first
        match unhandled.binary_search_by_key(&Reverse(ranges[0].begin), |(_, ranges)| {
            Reverse(ranges.last().unwrap().begin)
        }) {
            Ok(idx) | Err(idx) => {
                unhandled.insert(idx, (value_idx, ranges));
            }
        }
    }

    println!(
        "unhandled: {:?}",
        unhandled
            .iter()
            .rev()
            .map(|(value_idx, live_ranges)| {
                (
                    value_idx.debug(ctx, fun),
                    live_ranges
                        .iter()
                        .rev()
                        .map(|live_range| live_range.debug(ctx, fun))
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>()
    );

    // Intervals that are active in the current range (with registers assigned)
    let mut active: Vec<Allocation> = vec![];
    // Intervals that are not active in the current range
    let mut inactive: Vec<Allocation> = vec![];

    while let Some((current_value_idx, mut current_interval)) = unhandled.pop() {
        let LiveRange {
            begin: current_begin,
            end: current_end,
        } = current_interval.last().unwrap().clone();

        println!("--- NEW ALLOCATION");

        println!("current begin: {}, end: {}", current_begin, current_end);

        println!(
            "active: {:#?}",
            active
                .iter()
                .map(|alloc| alloc.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        println!(
            "inactive: {:#?}",
            inactive
                .iter()
                .map(|alloc| alloc.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        // Check for intervals in 'active' that are handled or inactive
        {
            let mut active_idx = 0;
            'next_active: while active_idx < active.len() {
                let a = &mut active[active_idx];
                if a.ranges[0].end < current_begin {
                    // Handled
                    active.remove(active_idx);
                } else {
                    // Pop ranges that are handled, then check if 'position' intersects with the
                    // next range in this 'active' interval. If it is then it'll stay as active,
                    // otherwise we'll move it to 'inactive'.
                    while let Some(next) = a.ranges.pop() {
                        if next.end < current_begin {
                            continue; // Range is handled and popped
                        } else {
                            let next_begin = next.begin;
                            a.ranges.push(next);
                            if next_begin > current_end {
                                // The range doesn't overlap with current
                                let r = active.remove(active_idx);
                                inactive.push(r);
                                continue 'next_active;
                            } else {
                                // The range overlaps with current; keep it
                                active_idx += 1;
                                continue 'next_active;
                            }
                        }
                    }

                    assert!(a.ranges.is_empty());
                    active.remove(active_idx);
                }
            }
        }

        println!("actives updated");

        println!(
            "active: {:#?}",
            active
                .iter()
                .map(|alloc| alloc.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        println!(
            "inactive: {:#?}",
            inactive
                .iter()
                .map(|alloc| alloc.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        // Check for intervals in 'inactive' that are handled or active
        {
            let mut inactive_idx = 0;
            'next_inactive: while inactive_idx < inactive.len() {
                let a = &mut inactive[inactive_idx];
                if a.ranges[0].end < current_begin {
                    // Handled
                    inactive.remove(inactive_idx);
                } else {
                    while let Some(next) = a.ranges.pop() {
                        if next.end < current_begin {
                            continue; // Range is handled and popped
                        } else {
                            let next_begin = next.begin;
                            a.ranges.push(next);
                            if next_begin > current_end {
                                // The range doesn't overlap with current; keep it
                                inactive_idx += 1;
                                continue 'next_inactive;
                            } else {
                                // The range overlaps with current; move it to active
                                let r = inactive.remove(inactive_idx);
                                active.push(r);
                                continue 'next_inactive;
                            }
                        }
                    }

                    assert!(a.ranges.is_empty());
                    inactive.remove(inactive_idx);
                }
            }
        }

        println!("inactives updated");

        println!(
            "active: {:#?}",
            active
                .iter()
                .map(|alloc| alloc.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        println!(
            "inactive: {:#?}",
            inactive
                .iter()
                .map(|alloc| alloc.debug(ctx, fun))
                .collect::<Vec<_>>()
        );

        //
        // Find a register for 'current'
        //

        // free_until_pos: maps registers to their next uses. Register N is available until
        // free_until_pos[N]. Initially all registers are available.
        let mut free_until_pos: [InstrIdx; 16] = [InstrIdx::from_u32(u32::MAX - 1); 16];

        println!(
            "free_until_pos before updating for actives: {:?}",
            free_until_pos
        );

        // Registers of 'active' intervals are not available
        for a in &active {
            println!("--- {:?}", a);
            free_until_pos[a.reg as usize] = InstrIdx::from_u32(0);
        }

        println!(
            "free_until_pos after updating for actives: {:?}",
            free_until_pos
        );

        // Registers of 'inactive' intervals are avaiable until the next instruction they'll be
        // used again
        for a in &inactive {
            // If an interval is 'inactive' then beginning the first range in the interval will be
            // greater than or equal to 'current', so it's enough to only check the first range to
            // find the next intersection of the inactive interval with 'current'

            let first_range = a.ranges.last().unwrap();
            free_until_pos[a.reg as usize] = min(free_until_pos[a.reg as usize], first_range.begin);
        }

        println!(
            "free_until_pos after updating for inactives: {:?}",
            free_until_pos
        );

        // Find the register with highest 'free_until_pos' for the current interval
        // TODO: We don't have to search here, we could store index of assignment with largest
        // InstrUdx above and use it.

        let mut max_free_until = free_until_pos[0].as_u32();
        let mut reg_idx = 0;
        for (reg_idx_, free_until) in free_until_pos[1..].iter().enumerate() {
            if free_until.as_u32() > max_free_until {
                max_free_until = free_until.as_u32();
                reg_idx = reg_idx_ + 1;
            }
        }

        println!("reg_idx: {}, free_until: {}", reg_idx, max_free_until);

        if max_free_until == 0 {
            // No register available without spilling
            todo!("spill");
        } else if current_interval[0].end.as_u32() < max_free_until {
            // Register available for the whole interval
            println!(
                "assign {:?} -> {:?}",
                current_value_idx.debug(ctx, fun),
                Reg::from(reg_idx)
            );

            assigns.push((current_value_idx, Reg::from(reg_idx)));

            let alloc = Allocation {
                value: current_value_idx,
                ranges: current_interval,
                reg: Reg::from(reg_idx),
            };
            active.push(alloc);
        } else {
            // Register available for the first part of the interval. Split the current interval.
            let mut handled: Vec<LiveRange> = vec![];
            while let Some(next_range) = current_interval.pop() {
                if next_range.end.as_u32() < max_free_until {
                    handled.push(next_range);
                } else {
                    // Put it back
                    current_interval.push(next_range);
                    handled.reverse();
                    break;
                }
            }

            assert!(!current_interval.is_empty());

            println!(
                "assign with split {:?} -> {:?}",
                current_value_idx.debug(ctx, fun),
                Reg::from(reg_idx)
            );
            assigns.push((current_value_idx, Reg::from(reg_idx)));
            let alloc = Allocation {
                value: current_value_idx,
                ranges: handled,
                reg: Reg::from(reg_idx),
            };
            active.push(alloc);

            // TODO: Generate instructions to save the value on stack and load it later

            // Put unhandled part back to the 'unhandled' set
            match unhandled.binary_search_by_key(
                &Reverse(current_interval.last().unwrap().begin),
                |(_, ranges)| Reverse(ranges.last().unwrap().begin),
            ) {
                Ok(idx) | Err(idx) => {
                    unhandled.insert(idx, (current_value_idx, current_interval));
                }
            }
        }
    }

    println!("reg alloc done");
    println!("unhandled: {:?}", unhandled);
    println!(
        "active: {:#?}",
        active
            .iter()
            .map(|alloc| alloc.debug(ctx, fun))
            .collect::<Vec<_>>()
    );
    println!(
        "inactive: {:?}",
        inactive
            .iter()
            .map(|alloc| alloc.debug(ctx, fun))
            .collect::<Vec<_>>()
    );

    println!(
        "assigns: {:?}",
        assigns
            .iter()
            .map(|(value_idx, reg)| (value_idx.debug(ctx, fun), reg))
            .collect::<Vec<_>>()
    );
    assigns
}

// Called when unable to assign a register to the current interval. Spills the current interval, or
// spills another interval and assigns the register to the current interval.
fn spill(
    fun: &Fun, current_value_idx: ValueIdx, current: &[LiveRange], active: &[Allocation],
    inactive: &[Allocation],
) {
    // next_use_pos: maps registers to their *next uses* after `current`.
    let mut next_use_pos: [InstrIdx; 16] = [InstrIdx::from_u32(u32::MAX - 1); 16];

    // Update next_use_pos for active intervals
    for Allocation { value, reg, .. } in active {
        if let Some(next) = next_use(fun, *value, current[0].begin) {
            let instr_idx = get_value_instr(fun, next);
            next_use_pos[*reg as usize] = min(next_use_pos[*reg as usize], instr_idx);
        }
    }

    // Update next_use_pos for inactive intervals that intersect with the current interval
    for Allocation { value, reg, ranges } in inactive {
        if !intersects(current, ranges) {
            continue;
        }

        if let Some(next) = next_use(fun, *value, current[0].begin) {
            let instr_idx = get_value_instr(fun, next);
            next_use_pos[*reg as usize] = min(next_use_pos[*reg as usize], instr_idx);
        }
    }

    // Find the register with highest 'next_use_pos'
    let mut max_next_use_pos: InstrIdx = next_use_pos[0];
    let mut reg_idx = 0;
    for (reg_idx_, free_until) in next_use_pos[1..].iter().enumerate() {
        if *free_until > max_next_use_pos {
            max_next_use_pos = *free_until;
            reg_idx = reg_idx_ + 1;
        }
    }

    if current[0].begin > max_next_use_pos {
        // All other intervals are used before 'current', so it is est to spill 'current'
        // TODO: I don't understand this code.. I think this branch won't ever be taken?

        // TODO: spill 'current', split its interval before its first use that requires a register
    } else {
        // TODO: Spill intervals that currently block 'reg'
        // TODO: Split inactive intervals for reg
    }

    // TODO: something something about fixed interval of reg???
}

fn get_value_instr(fun: &Fun, value: ValueIdx) -> InstrIdx {
    // If 'next' is a phi then we consider first instruction of the block as use site
    // (TODO: not sure about this)
    match &fun.values[value] {
        Value::Global(_) | Value::Arg(_) => {
            panic!("Global or arg as use site");
        }
        Value::Instr(instr_idx) => *instr_idx,
        Value::Phi(phi_idx) => {
            let phi = &fun.phis[*phi_idx];
            let block = &fun.blocks[phi.owner];
            block.first_instr
        }
    }
}

fn intersects(r1: &[LiveRange], r2: &[LiveRange]) -> bool {
    debug_assert!(!r1.is_empty());
    debug_assert!(r1.is_sorted());
    debug_assert!(!r2.is_empty());
    debug_assert!(r2.is_sorted());

    let mut r1_iter = r1.iter().peekable();
    let mut r2_iter = r2.iter().peekable();

    loop {
        match (r1_iter.peek(), r2_iter.peek()) {
            (None, _) | (_, None) => {
                return false;
            }
            (Some(r1_), Some(r2_)) => {
                if r2_.end < r1_.begin {
                    r2_iter.next();
                } else if r1_.end < r2_.begin {
                    r1_iter.next();
                } else {
                    if between(r1_.begin, r2_) || between(r1_.end, r2_) {
                        return true;
                    }
                }
            }
        }
    }
}

fn between(i: InstrIdx, r: &LiveRange) -> bool {
    i >= r.begin && i <= r.end
}

/// Does the given interval cover 'idx' ?
fn covers(interval: &[LiveRange], idx: InstrIdx) -> bool {
    match interval
        .binary_search_by(|LiveRange { begin, end: _ }| Reverse(begin).cmp(&Reverse(&idx)))
    {
        Ok(_) => true,
        Err(range_idx) => {
            if range_idx == interval.len() {
                false
            } else {
                interval[range_idx].end >= idx
            }
        }
    }
}

/// Find next use of a value
fn next_use(fun: &Fun, value: ValueIdx, instr: InstrIdx) -> Option<ValueIdx> {
    let use_sites = &fun.value_use_sites[value];
    match use_sites.binary_search(&value) {
        Ok(idx) => {
            // Value used in current position
            assert_eq!(fun.values[use_sites[idx]], Value::Instr(instr));
            Some(use_sites[idx])
        }
        Err(idx) => {
            if idx == use_sites.len() {
                // Value not used after `instr`
                None
            } else {
                Some(use_sites[idx])
            }
        }
    }
}

//
// Debug interface
//

use std::fmt;

pub struct AllocationDebug<'a> {
    alloc: &'a Allocation,
    ctx: &'a Ctx,
    fun: &'a Fun,
}

impl Allocation {
    pub fn debug<'a>(&'a self, ctx: &'a Ctx, fun: &'a Fun) -> AllocationDebug<'a> {
        AllocationDebug {
            alloc: self,
            ctx,
            fun,
        }
    }
}

impl<'a> fmt::Debug for AllocationDebug<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("Allocation");

        s.field("value", &self.alloc.value.debug(self.ctx, self.fun));
        s.field(
            "ranges",
            &NoAlternate(
                self.alloc
                    .ranges
                    .iter()
                    .rev()
                    .map(|range| range.debug(self.ctx, self.fun))
                    .collect::<Vec<_>>(),
            ),
        );
        s.field("reg", &self.alloc.reg);

        s.finish()
    }
}

pub struct LiveRangeDebug<'a> {
    range: &'a LiveRange,
    ctx: &'a Ctx,
    fun: &'a Fun,
}

impl LiveRange {
    pub fn debug<'a>(&'a self, ctx: &'a Ctx, fun: &'a Fun) -> LiveRangeDebug<'a> {
        LiveRangeDebug {
            range: self,
            ctx,
            fun,
        }
    }
}

impl<'a> fmt::Debug for LiveRangeDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.range.begin, self.range.end)
    }
}

#[test]
fn test_covers() {
    let r = |begin, end| LiveRange {
        begin: InstrIdx::from_u32(begin),
        end: InstrIdx::from_u32(end),
    };

    assert_eq!(
        covers(&vec![r(5, 10), r(2, 4)], InstrIdx::from_u32(1)),
        false
    );

    assert_eq!(
        covers(&vec![r(5, 10), r(2, 4),], InstrIdx::from_u32(2)),
        true
    );

    assert_eq!(
        covers(&vec![r(5, 10), r(2, 4),], InstrIdx::from_u32(3)),
        true
    );

    assert_eq!(
        covers(&vec![r(7, 10), r(2, 4),], InstrIdx::from_u32(6)),
        false
    );
}

#[test]
fn test_intersects() {
    let r = |begin, end| LiveRange {
        begin: InstrIdx::from_u32(begin),
        end: InstrIdx::from_u32(end),
    };

    {
        let r1: Vec<LiveRange> = vec![r(1, 2)];
        let r2: Vec<LiveRange> = vec![r(3, 4)];
        assert!(!intersects(&r1, &r2));
        assert!(!intersects(&r2, &r1));
    }

    {
        let r1: Vec<LiveRange> = vec![r(1, 2)];
        let r2: Vec<LiveRange> = vec![r(2, 3)];
        assert!(intersects(&r1, &r2));
        assert!(intersects(&r2, &r1));
    }

    {
        let r1: Vec<LiveRange> = vec![r(1, 2), r(5, 6)];
        let r2: Vec<LiveRange> = vec![r(3, 4)];
        assert!(!intersects(&r1, &r2));
        assert!(!intersects(&r2, &r1));
    }

    {
        let r1: Vec<LiveRange> = vec![r(1, 2), r(9, 10)];
        let r2: Vec<LiveRange> = vec![r(3, 4), r(5, 6), r(7, 8), r(9, 15)];
        assert!(intersects(&r1, &r2));
        assert!(intersects(&r2, &r1));
    }
}
