// Implements "Optimized Interval Splitting in a Linear Scan Register Allocator" my Wimmer et al.

// NOTE: We create new instructions (mov) when splitting intervals below, and `InstrIdx` of
// those instructions do not follow the convention that all instructions in a block that start with
// i1 and end with i2 are in range (i1 .. i2). This shouldn't break anything as the instructions we
// generate don't affect register allocation for remaining intervals, but it's still something to
// keep in mind for the passes that follow.

// TODO: Make is easier to keep range lists in sorted order, and searching in them.

use super::fun::Fun;
use super::instr::{InstrIdx, ValueIdx};
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

pub fn reg_alloc(mut live_intervals: LiveIntervalMap, ctx: &Ctx, fun: &Fun) {
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

        match unhandled.binary_search_by_key(&Reverse(ranges[0].begin), |(_, ranges)| {
            Reverse(ranges.last().unwrap().begin)
        }) {
            Ok(idx) => {
                // Another value here with the same begin position, doesn't matter which one
                // comes first
                unhandled.insert(idx, (value_idx, ranges));
            }
            Err(idx) => {
                unhandled.insert(idx, (value_idx, ranges));
            }
        }
    }

    println!("unhandled: {:?}", unhandled);

    let mut active: Vec<Allocation> = vec![];
    let mut inactive: Vec<Allocation> = vec![];
    let mut handled: Vec<Allocation> = vec![];

    while let Some((current_value_idx, mut current_interval)) = unhandled.pop() {
        let position = current_interval.last().unwrap().begin;

        // Check for intervals in 'active' that are handled or inactive
        {
            let mut active_idx = 0;
            while active_idx < active.len() {
                let a = &active[active_idx];
                if a.ranges[0].end < position {
                    let range = active.remove(active_idx);
                    handled.push(range);
                } else if !covers(&a.ranges, position) {
                    let range = active.remove(active_idx);
                    inactive.push(range);
                } else {
                    active_idx += 1;
                }
            }
        }

        // Check for intervals in 'inactive' that are handled or active
        {
            let mut inactive_idx = 0;
            while inactive_idx < inactive.len() {
                let a = &inactive[inactive_idx];
                if a.ranges[0].end < position {
                    let range = active.remove(inactive_idx);
                    handled.push(range);
                } else if covers(&a.ranges, position) {
                    let range = active.remove(inactive_idx);
                    active.push(range);
                } else {
                    inactive_idx += 1;
                }
            }
        }

        //
        // Find a register for 'current'
        //

        // free_until_pos: maps registers to their next uses. Register N is available until
        // free_until_pos[N]. Initially all registers are available.
        let mut free_until_pos: [InstrIdx; 16] = [InstrIdx::from_u32(u32::MAX - 1); 16];

        // Registers of 'active' intervals are not available
        for a in &active {
            free_until_pos[a.reg as usize] = InstrIdx::from_u32(0);
        }

        // Registers of 'inactive' intervals are avaiable until the next instruction they'll be
        // used again
        for a in &inactive {
            // If an interval is 'inactive' then beginning the first range in the interval will be
            // greater than or equal to 'current', so it's enough to only check the first range to
            // find the next intersection of the inactive interval with 'current'

            let first_range = a.ranges.last().unwrap();
            free_until_pos[a.reg as usize] = min(free_until_pos[a.reg as usize], first_range.begin);
        }

        // Find the register with highest 'free_until_pos' for the current interval
        // TODO: We don't have to search here, we could store index of assignment with largest
        // InstrUdx above and use it.

        let mut max_free_until = free_until_pos[0].as_u32();
        let mut reg_idx = 0;
        for (reg_idx_, free_until) in free_until_pos[1..].iter().enumerate() {
            if free_until.as_u32() > max_free_until {
                max_free_until = free_until.as_u32();
                reg_idx = reg_idx_;
            }
        }

        if max_free_until == 0 {
            // No register available without spilling
            todo!("spill");
        } else if current_interval[0].end.as_u32() < max_free_until {
            // Register available for the whole interval
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
        "handled: {:?}",
        handled
            .iter()
            .map(|alloc| alloc.debug(ctx, fun))
            .collect::<Vec<_>>()
    );
}

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
    assert_eq!(
        covers(
            &vec![
                LiveRange {
                    begin: InstrIdx::from_u32(5),
                    end: InstrIdx::from_u32(10)
                },
                LiveRange {
                    begin: InstrIdx::from_u32(2),
                    end: InstrIdx::from_u32(4)
                }
            ],
            InstrIdx::from_u32(1)
        ),
        false
    );

    assert_eq!(
        covers(
            &vec![
                LiveRange {
                    begin: InstrIdx::from_u32(5),
                    end: InstrIdx::from_u32(10)
                },
                LiveRange {
                    begin: InstrIdx::from_u32(2),
                    end: InstrIdx::from_u32(4)
                }
            ],
            InstrIdx::from_u32(2)
        ),
        true
    );

    assert_eq!(
        covers(
            &vec![
                LiveRange {
                    begin: InstrIdx::from_u32(5),
                    end: InstrIdx::from_u32(10)
                },
                LiveRange {
                    begin: InstrIdx::from_u32(2),
                    end: InstrIdx::from_u32(4)
                }
            ],
            InstrIdx::from_u32(3)
        ),
        true
    );

    assert_eq!(
        covers(
            &vec![
                LiveRange {
                    begin: InstrIdx::from_u32(7),
                    end: InstrIdx::from_u32(10)
                },
                LiveRange {
                    begin: InstrIdx::from_u32(2),
                    end: InstrIdx::from_u32(4)
                }
            ],
            InstrIdx::from_u32(6)
        ),
        false
    );
}
