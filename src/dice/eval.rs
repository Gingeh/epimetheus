use std::collections::{btree_map, BTreeMap};

use smallvec::SmallVec;
use smol_str::SmolStr;

use crate::dice::{
    lex::{Op, Token},
    parse::ParseIns,
    value::{array_partition_idx, LazyValue, RVal},
};

use super::{
    value::{Place, RRVal, ResolveError},
    vec_into,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Evaluator {
    vars: BTreeMap<SmolStr, RRVal>,
}

impl Evaluator {
    fn new() -> Self {
        Self {
            vars: Default::default(),
        }
    }

    pub fn var_get<'s>(&'s self, place: &Place) -> Result<&'s RRVal, ResolveError> {
        let mut placeref = self
            .vars
            .get(&place.varname)
            .ok_or_else(|| ResolveError::undef_var(place.clone()))?;
        for (ii, i) in place.indexes.iter().enumerate() {
            match placeref {
                RRVal::Array(a) => {
                    placeref = a
                        .get(*i as usize)
                        .ok_or_else(|| ResolveError::index_out_of_bounds(place.clone(), ii))?;
                }
                _ => return Err(ResolveError::index_into_invalid_type(place.clone(), ii)),
            }
        }
        Ok(placeref)
    }

    pub fn var_set<'s>(
        &'s mut self,
        place: &Place,
        val: RRVal,
    ) -> Result<&'s mut RRVal, ResolveError> {
        let place_entry = self.vars.entry(place.varname.clone());
        match place_entry {
            btree_map::Entry::Vacant(entry) => Ok(entry.insert(val)),
            btree_map::Entry::Occupied(entry) => {
                let mut placeref = entry.into_mut();
                for (ii, i) in place.indexes.iter().enumerate() {
                    match placeref {
                        RRVal::Array(a) => {
                            placeref = a.get_mut(*i as usize).ok_or_else(|| {
                                ResolveError::index_out_of_bounds(place.clone(), ii)
                            })?;
                        }
                        _ => return Err(ResolveError::index_into_invalid_type(place.clone(), ii)),
                    }
                }
                *placeref = val;
                Ok(placeref)
            }
        }
    }
}

impl ParseIns for Evaluator {
    type Value = LazyValue;

    async fn literal<'t>(&self, v: crate::dice::lex::Token<'t>) -> anyhow::Result<Self::Value> {
        match v {
            Token::Number(x) => Ok(LazyValue::Int(x.into())),
            Token::Char(c) => Ok(LazyValue::Char(c)),
            Token::Str(s) => Ok(LazyValue::Array(s.chars().map(LazyValue::Char).collect())),
            Token::Ident(i) => Ok(LazyValue::Place(Place {
                varname: SmolStr::new(i),
                indexes: SmallVec::new(),
            })),
            Token::Eof => anyhow::bail!("incomplete expression".to_string()),
            _ => anyhow::bail!("invalid literal `{}`", v),
        }
    }

    async fn binop(
        &mut self,
        left: Self::Value,
        right: Self::Value,
        c: Op,
    ) -> anyhow::Result<Self::Value> {
        macro_rules! deepres {
            ($l:ident, $op:ident, $r:ident) => {
                LazyValue::from(
                    $l.deep_resolve(self)
                        .await?
                        .$op($r.deep_resolve(self).await?)
                        .await,
                )
            };
        }
        match c {
            Op::Plus => Ok(deepres!(left, add, right)),
            Op::Minus => Ok(deepres!(left, sub, right)),
            Op::Star => Ok(deepres!(left, mul, right)),
            Op::Slash => Ok(deepres!(left, fdiv, right)),
            Op::Comma => match (left.resolve(self).await?, right.resolve(self).await?) {
                (RVal::Array(mut a), RVal::Array(mut b)) => {
                    a.append(&mut b);
                    Ok(LazyValue::Array(a))
                }
                (RVal::Array(mut a), v) => {
                    a.push(v.into());
                    Ok(LazyValue::Array(a))
                }
                (v, RVal::Array(mut a)) => {
                    a.insert(0, v.into());
                    Ok(LazyValue::Array(a))
                }
                (v, w) => Ok(LazyValue::Array(vec![v.into(), w.into()])),
            },
            Op::Semicolon =>
            /* lmfao */
            {
                Ok(right)
            }
            Op::Equal => Ok(deepres!(left, op_eq, right)),
            Op::Or => Ok(deepres!(left, op_or, right)),
            Op::And => {
                todo!()
            }
            Op::Assign => {
                if let LazyValue::Place(place) = left {
                    let new_value = right.deep_resolve(self).await?;
                    self.var_set(&place, new_value)?;
                    Ok(LazyValue::Place(place)) // hehe
                } else {
                    anyhow::bail!("attempt to assign to an rvalue instead of a lvalue");
                }
            }
            _ => anyhow::bail!("invalid infix operator `{}`", c.as_str()),
        }
    }

    async fn pfxop(&self, inner: Self::Value, c: Op) -> anyhow::Result<Self::Value> {
        match c {
            Op::Plus => Ok(inner),
            Op::Minus => Ok(inner.deep_resolve(self).await?.neg().await.into()),
            Op::Comma => {
                /* Enlist! */
                Ok(LazyValue::Array(vec![inner]))
            }
            Op::Hash => {
                // Array length.
                match inner {
                    LazyValue::Array(a) => Ok(LazyValue::Int(a.len().into())),
                    _ => anyhow::bail!("cannot apply length operator (`#`) to non-array"),
                }
            }
            _ => anyhow::bail!("invalid prefix operator `{}`", c.as_str()),
        }
    }

    async fn sfxop(&self, inner: Self::Value, c: Op) -> anyhow::Result<Self::Value> {
        match c {
            Op::Percent => Ok(inner
                .deep_resolve(self)
                .await?
                .fdiv(100.into())
                .await
                .into()),
            Op::Bang => {
                /* explode! */
                if let LazyValue::LazyDice {
                    num,
                    sides,
                    lowest_idx,
                    highest_idx,
                    mut explode,
                } = inner
                {
                    let l = sides.len();
                    return Ok(LazyValue::LazyDice {
                        num,
                        sides,
                        lowest_idx,
                        highest_idx,
                        explode: {
                            explode.push(RRVal::Int(l.into()));
                            explode
                        },
                    });
                }
                match inner.resolve(self).await? {
                    RVal::Int(_) => {
                        anyhow::bail!("factorial isn't implemented yet, sorry :P".to_string())
                    }
                    RVal::Float(_) => {
                        anyhow::bail!(
                            "floating point factorial isn't implemented yet, sorry :P".to_string()
                        )
                    }
                    RVal::Char(_) => anyhow::bail!("you can't explode a character".to_string()),
                    RVal::Array(_) => {
                        anyhow::bail!("the operator `!` is not defined on arrays yet".to_string())
                    }
                }
            }
            _ => anyhow::bail!("invalid suffix operator `{}`", c.as_str()),
        }
    }

    async fn dice(
        &mut self,
        num: Option<Self::Value>,
        sides_raw: Self::Value,
    ) -> anyhow::Result<Self::Value> {
        const DICE_LIMIT_SIDES: u32 = 65535;
        // TODO: large dice optimization
        let num: u32 = match num {
            Some(nv) => nv
                .resolve(self)
                .await?
                .into_i32()
                .and_then(|v| u32::try_from(v).map_err(|_| "negative number of dice".to_string()))
                .and_then(|v| {
                    if v == 0 {
                        Err("too few dice, must be at least 1".to_string())
                    } else if v > DICE_LIMIT_SIDES {
                        Err(format!("too many dice: {v} > {DICE_LIMIT_SIDES}"))
                    } else {
                        Ok(v)
                    }
                }),
            None => Ok(1),
        }
        .map_err(|e| anyhow::anyhow!("invalid number of dice: {:?}", e))?;
        if num == 0 {
            return Ok(LazyValue::Int(rug::Integer::ZERO));
        }
        let sides;
        if let LazyValue::Array(a) = sides_raw {
            sides = RRVal::deep_resolve_vec(a, self).await?;
        } else {
            let sides_num: u32 = sides_raw
                .resolve(self)
                .await?
                .into_i32()
                .and_then(|v| u32::try_from(v).map_err(|_| "negative number of sides".to_string()))
                .map_err(|e| anyhow::anyhow!("invalid number of sides: {:?}", e))
                .and_then(|v| {
                    if v > DICE_LIMIT_SIDES {
                        Err(anyhow::anyhow!("too many sides: {v} > {DICE_LIMIT_SIDES}"))
                    } else {
                        Ok(v)
                    }
                })?;
            sides = (1..=sides_num).map(|y| RRVal::Int(y.into())).collect();
        }
        Ok(LazyValue::LazyDice {
            num,
            sides,
            lowest_idx: 0,
            highest_idx: num - 1,
            explode: vec![],
        })
    }

    async fn keep_highest(
        &mut self,
        dice: Self::Value,
        keep: Self::Value,
    ) -> anyhow::Result<Self::Value> {
        let kh: u32 = keep
            .resolve(self)
            .await?
            .into_i32()
            .and_then(|v| v.try_into().map_err(|_| format!("is negative {v}")))
            .map_err(|e| anyhow::anyhow!("invalid keep-highest criterion: {e}"))?;
        match dice {
            LazyValue::LazyDice {
                num,
                sides,
                lowest_idx,
                highest_idx,
                explode,
            } => Ok(LazyValue::LazyDice {
                num,
                sides,
                lowest_idx: lowest_idx.max(highest_idx.saturating_sub(kh) + 1),
                highest_idx,
                explode,
            }),
            LazyValue::Int(_) => {
                anyhow::bail!("keep-highest operation is invalid on integers".to_string())
            }
            LazyValue::Place(_) => {
                anyhow::bail!("keep-highest operation is invalid on variable references".to_string())
            }
            LazyValue::Float(_) => {
                anyhow::bail!("keep-highest operation is invalid on numbers".to_string())
            }
            LazyValue::Char(_) => {
                anyhow::bail!("keep-highest operation is invalid on characters".to_string())
            }
            LazyValue::Array(a) => {
                let idx = a.len() - kh as usize;
                Ok(LazyValue::Array(vec_into(
                    array_partition_idx(RRVal::deep_resolve_vec(a, self).await?, idx, false)
                        .await?,
                )))
            }
        }
    }

    async fn keep_lowest(
        &mut self,
        dice: Self::Value,
        keep: Self::Value,
    ) -> anyhow::Result<Self::Value> {
        let kl: u32 = keep
            .resolve(self)
            .await?
            .into_i32()
            .and_then(|v| v.try_into().map_err(|_| format!("is negative {v}")))
            .map_err(|e| anyhow::anyhow!("invalid keep-lowest criterion: {e}"))?;
        match dice {
            LazyValue::LazyDice {
                num,
                sides,
                lowest_idx,
                highest_idx,
                explode,
            } => Ok(LazyValue::LazyDice {
                num,
                sides,
                lowest_idx,
                highest_idx: highest_idx.min((lowest_idx + kl).saturating_sub(1)),
                explode,
            }),
            LazyValue::Int(_) => {
                anyhow::bail!("keep-lowest operation is invalid on integers".to_string())
            }
            LazyValue::Float(_) => {
                anyhow::bail!("keep-lowest operation is invalid on numbers".to_string())
            }
            LazyValue::Char(_) => {
                anyhow::bail!("keep-lowest operation is invalid on characters".to_string())
            }
            LazyValue::Place(_) => {
                anyhow::bail!("keep-lowest operation is invalid on variable references".to_string())
            }
            LazyValue::Array(a) => {
                let idx = kl as usize;
                Ok(LazyValue::Array(vec_into(
                    array_partition_idx(RRVal::deep_resolve_vec(a, self).await?, idx, true).await?,
                )))
            }
        }
    }

    async fn explode(
        &mut self,
        dice: Self::Value,
        inner: Self::Value,
    ) -> anyhow::Result<Self::Value> {
        match dice {
            LazyValue::Int(_) => anyhow::bail!("cannot explode integers".to_string()),
            LazyValue::Float(_) => anyhow::bail!("cannot explode numbers".to_string()),
            LazyValue::Array(_) => anyhow::bail!("cannot explode arrays".to_string()),
            LazyValue::Char(_) => anyhow::bail!("cannot explode characters".to_string()),
            LazyValue::Place(_) => anyhow::bail!("cannot explode variable references".to_string()),
            LazyValue::LazyDice {
                num,
                sides,
                lowest_idx,
                highest_idx,
                mut explode,
            } => Ok(LazyValue::LazyDice {
                num,
                sides,
                lowest_idx,
                highest_idx,
                explode: {
                    let mut res = match inner.deep_resolve(self).await? {
                        RRVal::Array(a) => a,
                        r => vec![r],
                    };
                    explode.append(&mut res);
                    explode
                },
            }),
        }
    }

    async fn mk_array(&mut self, arr: Vec<Self::Value>) -> anyhow::Result<Self::Value> {
        Ok(LazyValue::Array(arr))
    }
}

pub async fn eval(s: &str) -> anyhow::Result<RRVal> {
    let (evaluator, val) = crate::dice::parse::run_parser(s, Evaluator::new()).await?;
    let rrval = val.deep_resolve(&evaluator).await?;
    Ok(rrval)
}

#[tokio::test]
async fn eval_positive_test() {
    macro_rules! good {
        ($x:expr, $y:literal / $z:literal) => {
            let m;
            if let RRVal::Float(m1) = eval($x).await.unwrap() {
                m = m1;
            } else {
                panic!();
            }
            let r = ($y / $z);
            if (m - r).abs() >= f64::EPSILON {
                panic!("{} != {}", m, r);
            }
        };
        ($x:expr, $y:literal) => {
            assert_eq!(eval($x).await.unwrap().into_i32().unwrap(), $y)
        };
        ($x:expr, #$y:expr) => {
            assert_eq!(eval($x).await.unwrap(), $y.into())
        };
    }

    good!("2", 2);
    good!("414", 414);
    good!("3+4", 7);
    good!("3-4", -1);
    good!("3+4+5-6+7", 13);
    good!("3*4", 12);
    good!("1+2*3", 7);
    good!("1*2+3", 5);
    good!("1+2*3+4", 11);
    good!("1*2+3*4", 14);
    good!("-3", -3);
    good!("-3+4", 1);
    good!("+3-4", -1);
    good!("++++++++++3-4", -1);
    good!("++++++++++3-++++++++4", -1);
    good!("--3 - --4", -1);
    good!("3%", 3. / 100.);
    good!("4+3%", 403. / 100.);
    good!("3%+4", 403. / 100.);
    good!("3%%%", 3. / 1_000_000.);
    good!("(3+4)*5", 35);
    good!("5*(3+4)", 35);
    good!("-(3+4)", -7);
    good!("++++(3+4)%", 7. / 100.);

    good!("3d4", 7);
    good!("15d1", 15);
    good!("65535d65535", 2143601837);
    good!("-3d4+7", 0);

    good!("d2", 2);
    good!("d2+d2", 4);
    good!("2d2", 4);
    good!("10d2", 15);

    good!("4d1KH3", 3);
    good!("4d1K0", 0);
    good!("15d1h1", 1);
    good!("3d4KH2", 6);
    good!("3d4KL2", 4);

    good!("2d2!", 8);
    good!("2d2!(2)!", 8);
    good!("2d2!(,2)!", 8);

    good!("d3!", 5);
    good!("2d3", 5);
    good!("2d3!", 6);
    good!("2d3!(2,3)!", 12);

    good!("d4!", 3);
    good!("d4!(3)!", 7);

    good!(",2", #vec![2]);
    good!("1,2", #vec![1,2]);
    good!("1,2,3", #vec![1,2,3]);
    good!(",4-,3", #vec![vec![1]]);

    good!("12d6; d4", 3);
    good!("d4;d4;d4;d4;d4", 3);

    good!("d(,2)", 2);
    good!("d(0,0,0,0)", 0);
    good!("d(0,0,0,0,0,7,0,0,0,0,0)", 0);

    good!("[1,2]", #vec![1,2]);
    good!("[1]", #vec![1]);
    good!("[]", #Vec::<i32>::new());
    good!("[1,]", #vec![1]);
    good!("[1, 2,]", #vec![1,2]);

    good!(r#""unstring""#, #"unstring");
    good!(r#"d["left", "right", "up", "down"]"#, #"up");
    // This is *intentional*. (For now.)
    good!("d[]", 0);
    good!("d0", 0);

    good!("(3/4)*100", 75);
    good!("(1/2)*100", 50);

    good!("v=4; v", 4);
    good!("x=10000; x=1; x d x", 1);
    good!("y=3; x=y=4; [x,y]", #vec![4,4]);
    good!("#,4", 1);
    good!("#[1,2,3]", 3);
    good!(r#"#"string""#, 6);

    // - fuzzing-based tests -
    good!("0d[5,6,7]", 0);
    // special NaN check lmao
    let nanres = eval("30d(0/3,0/0,0)").await.unwrap();
    let RRVal::Float(nanres) = nanres else { panic!("non-NaN value {nanres}") };
    assert!(nanres.is_nan(), "non-NaN value {nanres}");
}

#[tokio::test]
async fn eval_negative_test() {
    macro_rules! bad {
        ($x:expr) => {
            let e = eval($x).await;
            if e.is_ok() {
                panic!("no error thrown for {:?}", e.unwrap());
            }
        };
    }

    bad!("4810954d1093491"); // dice x > 65535
    bad!("2 2");
    bad!("$2");
    bad!("2$");
    bad!("#2");
    bad!("2 = 2");
    bad!("x");
}
