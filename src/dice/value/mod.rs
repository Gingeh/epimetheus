mod lazy_value;
pub use lazy_value::LazyValue;

mod rval;
pub use rval::RVal;

mod rrval;
pub use rrval::RRVal;

use rug::Integer;

fn cmp_rrvals(lhs: &RRVal, rhs: &RRVal) -> std::cmp::Ordering {
    use std::cmp::Ordering::*;
    match (lhs, rhs) {
        (RRVal::Int(n), RRVal::Int(m)) => n.cmp(m),
        (RRVal::Int(n), RRVal::Float(f)) => n.partial_cmp(f).unwrap_or(Greater), /* All integers > NaN */
        (RRVal::Float(f), RRVal::Int(n)) => f.partial_cmp(n).unwrap_or(Less), /* NaN < all integers */
        (RRVal::Char(c), RRVal::Char(d)) => c.cmp(d),
        (RRVal::Char(c), RRVal::Int(n)) => (*c as u32).partial_cmp(n).unwrap(),
        (RRVal::Int(n), RRVal::Char(c)) => n.partial_cmp(&(*c as u32)).unwrap(),
        (RRVal::Float(f), RRVal::Char(c)) => f.partial_cmp(&(*c as u32 as f64)).unwrap_or(Less),
        (RRVal::Char(c), RRVal::Float(f)) => (*c as u32 as f64).partial_cmp(f).unwrap_or(Greater),
        (RRVal::Float(a), RRVal::Float(b)) => a.partial_cmp(b).unwrap_or_else(|| {
            if a.is_nan() {
                if b.is_nan() {
                    // a, b are both NaN
                    Equal // lmfao
                } else {
                    // a is the only NaN
                    // all is greater than NaN
                    Greater
                }
            } else {
                // b is the only NaN
                // NaN is less than all
                Less
            }
        }),
        (RRVal::Array(a), RRVal::Array(b)) => {
            // lexographic comparison
            if a.len() != b.len() {
                return a.len().cmp(&b.len());
            }
            todo!()
        }
        (v, RRVal::Array(a)) => {
            // compare by first element
            if let Some(first) = a.first() {
                cmp_rrvals(v, first)
            } else {
                // empty arrays are below everything else
                Less
            }
        }
        (RRVal::Array(a), v) => {
            // compare by first element
            if let Some(first) = a.first() {
                cmp_rrvals(first, v)
            } else {
                // empty arrays are below everything else
                Greater
            }
        }
    }
}

pub async fn resolve_dice(
    num: u32,
    sides: Vec<RRVal>,
    lowest_idx: u32,
    highest_idx: u32,
    explode: Vec<RRVal>,
) -> RRVal {
    if sides.is_empty() || highest_idx < lowest_idx {
        return RRVal::Int(Integer::ZERO);
    }
    use rand::distributions::{Distribution, Uniform};
    let mut rng = crate::dice::get_rng();
    let between = Uniform::from(0..sides.len());
    async fn do_explode(
        rng: &mut (impl rand::Rng + Send),
        sides: &[RRVal],
        between: &Uniform<usize>,
        explode: &[RRVal],
    ) -> RRVal {
        let mut sum: Option<RRVal> = None;
        /* do-while loop, cough cough... */
        while {
            let i = between.sample(rng);
            let x = &sides[i];
            if let Some(s) = sum {
                sum = Some(s.add(x.clone()).await);
            } else {
                sum = Some(x.clone());
            }
            explode.contains(x)
        } {
            // just in case.
            crate::util::yield_point().await;
        }
        sum.unwrap_or(RRVal::Int(Integer::ZERO))
    }
    if lowest_idx == 0 && highest_idx as usize == sides.len() - 1 {
        let mut sum: Option<RRVal> = None;
        for _ in 0..num {
            let sample = if !explode.is_empty() {
                do_explode(&mut rng, &sides, &between, &explode).await
            } else {
                let i = between.sample(&mut rng);
                sides[i].clone()
            };
            if let Some(s) = sum {
                sum = Some(s.add(sample).await);
            } else {
                sum = Some(sample);
            }
        }
        sum.unwrap_or(RRVal::Int(Integer::ZERO))
    } else {
        let mut res = Vec::new();
        res.reserve_exact(num as usize);
        for _ in 0..num {
            let sample = if !explode.is_empty() {
                do_explode(&mut rng, &sides, &between, &explode).await
            } else {
                let i = between.sample(&mut rng);
                sides[i].clone()
            };
            res.push(sample);
        }
        res.sort_unstable_by(cmp_rrvals);
        let mut sum: Option<RRVal> = None;
        for item in res.drain(lowest_idx as usize..=highest_idx as usize) {
            if let Some(s) = sum {
                sum = Some(s.add(item).await);
            } else {
                sum = Some(item);
            }
        }
        sum.unwrap_or(RRVal::Int(Integer::ZERO))
    }
}

impl From<i32> for LazyValue {
    fn from(value: i32) -> Self {
        Self::Int(value.into())
    }
}

impl From<f64> for LazyValue {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

pub async fn array_partition_idx(
    a: Vec<RRVal>,
    idx: usize,
    take_lower: bool,
) -> Result<Vec<RRVal>, String> {
    let mut idxs: Vec<usize> = (0..a.len()).collect();
    let low_idx = idxs
        .select_nth_unstable_by(idx, |i, j| cmp_rrvals(&a[*i], &a[*j]))
        .1;
    let low_elem = &a[*low_idx].clone();
    Ok(a.into_iter()
        .filter(|a| {
            let is_lower = cmp_rrvals(a, low_elem) == std::cmp::Ordering::Less;
            if take_lower {
                is_lower
            } else {
                !is_lower
            }
        })
        .collect())
}

pub fn escape_string_for_discord_inplace(inp: &str, s: &mut String) {
    s.push_str("``"); // outer code formatting to prevent abusing mentions
    s.push('"');
    for c in inp.chars() {
        if c == '`' {
            s.push('`');
            s.push('\u{200b}'); // zero-width space, prevent "``"
        } else {
            for x in c.escape_default() {
                s.push(x);
            }
        }
    }
    s.push('"');
    s.push_str("``"); // outer code formatting to prevent abusing mentions
}

pub fn escape_string_for_discord(inp: &str) -> String {
    let mut s = String::new();
    escape_string_for_discord_inplace(inp, &mut s);
    s
}
