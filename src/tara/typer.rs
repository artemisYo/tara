use std::collections::BTreeMap as Map;
use std::rc::Rc;

use crate::{message, misc::Ivec, Provenance};

use super::{
    uir::{self, ExprId, FuncId, Type, TypeId, Typekind},
    Tara,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct In {
    pub i: uir::Id,
}

#[derive(Clone)]
pub struct Out {
    pub substitutions: Rc<Map<usize, TypeId>>,
}

impl Tara {
    pub fn typeck(&mut self, i: In) -> Out {
        match self.typecheck.get(&i) {
            Some(Some(o)) => o.clone(),
            Some(None) => panic!("typeck entered a cycle!"),
            None => {
                self.typecheck.insert(i, None);
                let data = typeck(self, i);
                self.typecheck.insert(i, Some(data));
                self.typecheck.get(&i).cloned().unwrap().unwrap()
            }
        }
    }
}

pub enum Constraint {
    Unify(Type, Type),
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum ControlFlow {
    Return,
    Break,
}
impl ControlFlow {
    fn minimum(self, other: Self) -> Self {
        use ControlFlow::*;
        match (self, other) {
            (Break, _) | (_, Break) => Break,
            (Return, Return) => Return,
        }
    }
}

fn typeck(tara: &mut Tara, i: In) -> Out {
    let constraints = items(i.i, tara);
    let substitutions = solve_constraints(constraints, tara);
    Out {
        substitutions: Rc::new(substitutions),
    }
}

fn items(i: uir::Id, tara: &mut Tara) -> Vec<Constraint> {
    match tara.uir_items[i] {
        uir::Item::Typecase(_) => vec![],
        uir::Item::Typedecl(_) => vec![],
        uir::Item::Function(_) => funcs(i.into_func(), tara),
        uir::Item::Namespace(_) => {
            let is = tara.uir_interfaces[i].into_namespace().items.clone();
            for &i in is.values() {
                tara.typeck(In { i });
            }
            vec![]
        }
    }
}

fn funcs(i: FuncId, tara: &mut Tara) -> Vec<Constraint> {
    let mut constraints = vec![];
    let body = tara.uir_items[i].body;
    if let Ok(t) = body.typeck(i, &mut constraints, tara) {
        constraints.push(Constraint::Unify(
            tara.uir_interfaces[i].typ.ret(&tara.uir_types).unwrap(),
            t,
        ));
    }
    constraints
}

impl ExprId {
    fn typeck(
        self,
        owner: FuncId,
        cs: &mut Vec<Constraint>,
        tara: &mut Tara,
    ) -> Result<Type, ControlFlow> {
        let loc = tara[(owner, self)].loc;
        let old_type = tara[(owner, self)].typ;
        let typ = match tara[(owner, self)].kind {
            uir::Exprkind::Poison => std::process::exit(1),
            uir::Exprkind::If { cond, smash, pass } => {
                let cond_t = cond.typeck(owner, cs, tara)?;
                let cond_loc = tara[(owner, cond)].loc;
                let bool_t = Type::simple(&mut tara.uir_types, Typekind::Bool, cond_loc);
                cs.push(Constraint::Unify(cond_t, bool_t));
                let smash = smash.typeck(owner, cs, tara);
                let pass = pass.typeck(owner, cs, tara);
                let (smash, pass) = match (smash, pass) {
                    (Err(s), Err(p)) => return Err(s.minimum(p)),
                    (Err(_), Ok(k)) | (Ok(k), Err(_)) => (k, tara[(owner, self)].typ),
                    (Ok(s), Ok(p)) => (s, p),
                };
                cs.push(Constraint::Unify(smash, pass));
                smash
            }
            uir::Exprkind::Call { func, args } => {
                let func = func.typeck(owner, cs, tara)?;
                dbg!(&tara.uir_types[func.kind]);
                let args = args.typeck(owner, cs, tara)?;
                dbg!(&tara.uir_types[args.kind]);
                let ret = old_type;
                dbg!(&tara.uir_types[ret.kind]);
                let unapp = Type::func(&mut tara.uir_types, args, ret, loc);
                dbg!(&tara.uir_types[unapp.kind]);
                cs.push(Constraint::Unify(func, unapp));
                ret
            }
            uir::Exprkind::Builtin(builtinkind) => builtins(builtinkind, loc, tara),
            uir::Exprkind::Tuple(ref expr_ids) => {
                let mut fields = Vec::new();
                for &e in expr_ids.clone().iter() {
                    fields.push(e.typeck(owner, cs, tara)?);
                }
                Type::tup(&mut tara.uir_types, &fields, loc)
            }
            uir::Exprkind::Loop(id) => {
                if Err(ControlFlow::Return) == id.typeck(owner, cs, tara) {
                    return Err(ControlFlow::Return);
                }
                tara[(owner, self)].typ
            }
            uir::Exprkind::Bareblock(ref ids) => {
                let ids = ids.clone();
                let mut t = Type::unit(&mut tara.uir_types, loc);
                for e in ids.iter() {
                    t = e.typeck(owner, cs, tara)?;
                }
                t
            }
            uir::Exprkind::Recall(Ok(bid)) => tara[(owner, bid)].typ,
            uir::Exprkind::Recall(Err(id)) => tara.item_type(id, Some(loc)),
            uir::Exprkind::Number(_) => Type::simple(&mut tara.uir_types, Typekind::Int, loc),
            uir::Exprkind::String(_) => Type::simple(&mut tara.uir_types, Typekind::String, loc),
            uir::Exprkind::Bool(_) => Type::simple(&mut tara.uir_types, Typekind::Bool, loc),
            uir::Exprkind::Arguments => tara[(owner, self)].typ,
            uir::Exprkind::Let(bid, eid) => {
                let bind = tara[(owner, bid)].typ;
                let expr = eid.typeck(owner, cs, tara)?;
                cs.push(Constraint::Unify(bind, expr));
                tara.unit(loc)
            }
            uir::Exprkind::Assign(bid, eid) => {
                if let uir::Bindkind::Name(n, false) = tara[(owner, bid)].kind {
                    let decl_loc = tara[(owner, bid)].loc;
                    tara.report(
                        message!(error @ loc => "Cannot reassign an immutable variable!"),
                        &[message!( note @ decl_loc => "'{}' is declared here!", n.0)],
                    );
                    std::process::exit(1);
                }
                let bind = tara[(owner, bid)].typ;
                let expr = eid.typeck(owner, cs, tara)?;
                cs.push(Constraint::Unify(bind, expr));
                tara.unit(loc)
            }
            uir::Exprkind::Break { val, target } => {
                let val = val.typeck(owner, cs, tara)?;
                let target = tara[(owner, target)].typ;
                cs.push(Constraint::Unify(val, target));
                return Err(ControlFlow::Break);
            }
            uir::Exprkind::Return(val) => {
                let val = val.typeck(owner, cs, tara)?;
                let target = tara.uir_interfaces[owner].typ.ret(&tara.uir_types).unwrap();
                cs.push(Constraint::Unify(val, target));
                return Err(ControlFlow::Return);
            }
            uir::Exprkind::Const(eid) => {
                eid.typeck(owner, cs, tara)?;
                tara.unit(loc)
            }
        };
        // probably not always needed but included for sanity
        cs.push(Constraint::Unify(typ, old_type));
        Ok(typ)
    }
}

fn builtins(kind: uir::Builtinkind, l: Provenance, tara: &mut Tara) -> Type {
    let typevec = &mut tara.uir_types;
    let int_t = Type::simple(typevec, Typekind::Int, l);
    let bool_t = Type::simple(typevec, Typekind::Bool, l);
    let string_t = Type::simple(typevec, Typekind::String, l);
    let ints = [int_t, int_t, int_t, int_t, int_t, int_t, int_t];
    let int2_t = Type::tup(typevec, &ints[0..2], l);
    let int7_t = Type::tup(typevec, &ints, l);
    match kind {
        uir::Builtinkind::Add
        | uir::Builtinkind::Sub
        | uir::Builtinkind::Mul
        | uir::Builtinkind::Div
        | uir::Builtinkind::Mod
        | uir::Builtinkind::And
        | uir::Builtinkind::Or
        | uir::Builtinkind::Xor
        | uir::Builtinkind::ShLeft
        | uir::Builtinkind::ShRight => Type::func(typevec, int2_t, int_t, l),
        uir::Builtinkind::Not | uir::Builtinkind::Negate => Type::func(typevec, int_t, int_t, l),
        uir::Builtinkind::CmpEq
        | uir::Builtinkind::CmpNE
        | uir::Builtinkind::CmpGt
        | uir::Builtinkind::CmpLt
        | uir::Builtinkind::CmpGE
        | uir::Builtinkind::CmpLE => Type::func(typevec, int2_t, bool_t, l),
        uir::Builtinkind::Syscall => Type::func(typevec, int7_t, int_t, l),
        uir::Builtinkind::PtrToInt => Type::func(typevec, string_t, int_t, l),
        uir::Builtinkind::IntToPtr => Type::func(typevec, int_t, string_t, l),
    }
}

fn solve_constraints(mut cs: Vec<Constraint>, tara: &mut Tara) -> Map<usize, TypeId> {
    let mut out = Map::new();
    while let Some(Constraint::Unify(a, b)) = cs.pop() {
        match (&tara.uir_types[a.kind], &tara.uir_types[b.kind]) {
            (Typekind::Var(a), _) => {
                out.insert(*a, b.kind);
                substitute(&mut cs, &mut tara.uir_types, &out);
            }
            (_, Typekind::Var(b)) => {
                out.insert(*b, a.kind);
                substitute(&mut cs, &mut tara.uir_types, &out);
            }
            (Typekind::Bundle(ref ks_a), Typekind::Bundle(ref ks_b)) => {
                for (&k_a, &k_b) in ks_a.clone().iter().zip(ks_b.clone().iter()) {
                    cs.push(Constraint::Unify(
                        Type {
                            kind: k_a,
                            loc: a.loc,
                        },
                        Type {
                            kind: k_b,
                            loc: b.loc,
                        },
                    ));
                }
            }
            (Typekind::Call { args: a1, func: f1 }, Typekind::Call { args: a2, func: f2 }) => {
                cs.push(Constraint::Unify(
                    Type {
                        kind: *f1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *f2,
                        loc: b.loc,
                    },
                ));
                cs.push(Constraint::Unify(
                    Type {
                        kind: *a1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *a2,
                        loc: b.loc,
                    },
                ));
            }
            (Typekind::Func { args: a1, ret: r1 }, Typekind::Func { args: a2, ret: r2 }) => {
                cs.push(Constraint::Unify(
                    Type {
                        kind: *a1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *a2,
                        loc: b.loc,
                    },
                ));
                cs.push(Constraint::Unify(
                    Type {
                        kind: *r1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *r2,
                        loc: b.loc,
                    },
                ));
            }
            (ref a_k, ref b_k) if a_k == b_k => {}
            (ref a_k, ref b_k) if a_k != b_k => {
                dbg!(a, b);
                tara.report(
                    message!(error => "Could not unify types!"),
                    &[
                        message!(
                            note @ a.loc =>
                            "Type '{}' originates here:", a.fmt(&tara.uir_interfaces, &tara.uir_types)
                        ),
                        message!(
                            note @ b.loc =>
                            "Type '{}' originates here:", b.fmt(&tara.uir_interfaces, &tara.uir_types)
                        ),
                    ],
                );
                // TODO: poisoning or smt
                std::process::exit(1);
            }
            _ => unreachable!(),
        }
    }
    out
}

fn substitute(
    cs: &mut [Constraint],
    typevec: &mut Ivec<uir::TypeId, Typekind>,
    map: &Map<usize, TypeId>,
) {
    for c in cs.iter_mut() {
        match c {
            Constraint::Unify(a, b) => {
                a.kind.replace(typevec, map);
                b.kind.replace(typevec, map);
            }
        }
    }
}
