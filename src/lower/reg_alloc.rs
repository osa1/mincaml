// Implements "Optimized Interval Splitting in a Linear Scan Register Allocator" my Wimmer et al.

use super::instr::{InstrIdx, ValueIdx};
use super::liveness::{LiveInterval, LiveIntervalMap, LiveRange};

use std::cmp::Reverse;
use std::mem::replace;

#[rustfmt::skip]
#[derive(Debug, Clone, Copy)]
pub enum Reg {
    RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8,
    R9, R10, R11, R12, R13, R14, R15,
}

pub fn reg_alloc(mut live_intervals: LiveIntervalMap) {
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
            Reverse(ranges[0].begin)
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

    let mut active: Vec<(ValueIdx, Vec<LiveRange>)> = vec![];
    let mut inactive: Vec<(ValueIdx, Vec<LiveRange>)> = vec![];
    let mut handled: Vec<(ValueIdx, Vec<LiveRange>)> = vec![];

    while let Some((current_value_idx, current_interval)) = unhandled.pop() {
        let position = current_interval.last().unwrap().begin;

        // Check for intervals in 'active' that are handled or inactive
        {
            let mut active_idx = 0;
            while active_idx < active.len() {
                let (_, ranges) = &active[active_idx];
                if ranges[0].end < position {
                    let range = active.remove(active_idx);
                    handled.push(range);
                } else if !covers(ranges, position) {
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
                let (_, ranges) = &inactive[inactive_idx];
                if ranges[0].end < position {
                    let range = active.remove(inactive_idx);
                    handled.push(range);
                } else if covers(ranges, position) {
                    let range = active.remove(inactive_idx);
                    active.push(range);
                } else {
                    inactive_idx += 1;
                }
            }
        }

        // Find a register for 'current'
        // TODO
    }
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
