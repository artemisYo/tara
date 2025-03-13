use std::collections::BTreeMap as Map;
use std::rc::Rc;

use crate::{misc::Ivec, report, Message, Provenance};

use super::{
    uir::{self, ExprId, Type, TypeId, Typekind},
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

struct Context<'a> {
    items: &'a mut Ivec<uir::Id, uir::Function>,
    typevec: &'a mut Ivec<uir::TypeId, uir::Typekind>,
    modules: &'a Ivec<super::ModuleId, super::Module>,
    func: uir::Id,
    constraints: Vec<Constraint>,
}

#[derive(PartialEq, Eq)]
enum ControlFlow {
    Return,
    Break,
}

fn typeck(tara: &mut Tara, i: In) -> Out {
    let mut ctx = Context {
        constraints: Vec::new(),
        func: i.i,
        items: &mut tara.uir_items,
        typevec: &mut tara.uir_types,
        modules: &mut tara.modules,
    };
    let body = ctx.items[i.i].body;
    if let Ok(t) = ctx.expressions(body) {
        let item = &mut ctx.items[ctx.func];
        ctx.constraints.push(Constraint::Unify(
            item.typ.ret(ctx.typevec).unwrap().clone(),
            t,
        ));
    }
    let substitutions = solve_constraints(ctx.typevec, ctx.modules, ctx.constraints);
    Out {
        substitutions: Rc::new(substitutions),
    }
}

impl Context<'_> {
    fn add_constraint(&mut self, Constraint::Unify(a, b): Constraint) {
        self.constraints.push(Constraint::Unify(a, b));
    }
    fn item(&mut self) -> &mut uir::Function {
        &mut self.items[self.func]
    }
    fn locals(&mut self) -> &mut uir::LocalVec {
        &mut self.items[self.func].locals
    }

    fn expressions(&mut self, e: ExprId) -> Result<Type, ControlFlow> {
        let loc = self.locals()[e].loc;
        let old_type = self.locals()[e].typ.clone();
        let typ = match self.locals()[e].kind.clone() {
            uir::Exprkind::Poison => self.locals()[e].typ.clone(),
            uir::Exprkind::If { cond, smash, pass } => {
                let cond_t = self.expressions(cond)?;
                let cond_loc = self.locals()[cond].loc;
                let bool_t = Type::simple(self.typevec, Typekind::Bool, cond_loc);
                self.add_constraint(Constraint::Unify(cond_t, bool_t));
                let smash = self.expressions(smash);
                let pass = self.expressions(pass);
                let (smash, pass) = match (smash, pass) {
                    (Err(ControlFlow::Break), Err(_)) | (Err(_), Err(ControlFlow::Break)) => {
                        return Err(ControlFlow::Break)
                    }
                    (Err(_), Err(_)) => return Err(ControlFlow::Return),
                    (Err(_), Ok(k)) | (Ok(k), Err(_)) => (k, self.locals()[e].typ.clone()),
                    (Ok(s), Ok(p)) => (s, p),
                };
                self.add_constraint(Constraint::Unify(smash.clone(), pass));
                smash
            }
            uir::Exprkind::Call { func, args } => {
                let func = self.expressions(func)?;
                let args = self.expressions(args)?;
                let ret = old_type.clone();
                let unapp = Type::func(self.typevec, &args, &ret, loc);
                self.add_constraint(Constraint::Unify(func, unapp));
                ret
            }
            uir::Exprkind::Builtin { builtin, args, loc } => {
                let builtin = self.builtins(builtin, loc);
                let args = self.expressions(args)?;
                let ret = old_type.clone();
                let unapp = Type::func(self.typevec, &args, &ret, loc);
                self.add_constraint(Constraint::Unify(builtin, unapp));
                ret
            }
            uir::Exprkind::Tuple(expr_ids) => {
                let mut fields = Vec::new();
                for e in expr_ids.iter() {
                    fields.push(self.expressions(*e)?);
                }
                Type::tup(self.typevec, &fields, loc)
            }
            uir::Exprkind::Loop(id) => {
                // explicitly catch breaks
                if Err(ControlFlow::Return) == self.expressions(id) {
                    return Err(ControlFlow::Return);
                }
                self.locals()[e].typ.clone()
            }
            uir::Exprkind::Bareblock(expr_ids) => {
                let mut t = Type::unit(self.typevec, loc);
                for e in expr_ids.iter() {
                    t = self.expressions(*e)?;
                }
                t
            }
            uir::Exprkind::Recall(Ok(binding_id)) => self.locals()[binding_id].typ.clone(),
            uir::Exprkind::Recall(Err(item_id)) => self.items[item_id].typ.clone(),
            uir::Exprkind::Number(_) => Type::simple(self.typevec, Typekind::Int, loc),
            uir::Exprkind::String(_) => Type::simple(self.typevec, Typekind::String, loc),
            uir::Exprkind::Bool(_) => Type::simple(self.typevec, Typekind::Bool, loc),
            uir::Exprkind::Arguments => self.locals()[e].typ.clone(),
            uir::Exprkind::Let(binding_id, expr_id) => {
                let bind = self.locals()[binding_id].typ.clone();
                let expr = self.expressions(expr_id)?;
                self.add_constraint(Constraint::Unify(bind, expr));
                Type::unit(self.typevec, loc)
            }
            uir::Exprkind::Assign(binding_id, expr_id) => {
                if let uir::Bindkind::Name(n, false) = self.locals()[binding_id].kind {
                    let decl_loc = self.locals()[binding_id].loc;
                    report(
                        self.modules,
                        Message::error("Cannot reassign an immutable variable!", Some(loc)),
                        &[Message::note(
                            &format!("'{}' is declared here!", n.0),
                            Some(decl_loc),
                        )],
                    );
                }
                let bind = self.locals()[binding_id].typ.clone();
                let expr = self.expressions(expr_id)?;
                self.add_constraint(Constraint::Unify(bind, expr));
                Type::unit(self.typevec, loc)
            }
            uir::Exprkind::Break { val, target } => {
                let val = self.expressions(val)?;
                let target = self.locals()[target].typ.clone();
                self.add_constraint(Constraint::Unify(val, target));
                return Err(ControlFlow::Break);
            }
            uir::Exprkind::Return(expr_id) => {
                let val = self.expressions(expr_id)?;
                let target = self.item().typ.clone().ret(self.typevec).unwrap().clone();
                self.add_constraint(Constraint::Unify(val, target));
                return Err(ControlFlow::Return);
            }
            uir::Exprkind::Const(expr_id) => {
                self.expressions(expr_id)?;
                Type::unit(self.typevec, loc)
            }
        };
        // probably not always needed but included for sanity
        self.add_constraint(Constraint::Unify(typ.clone(), old_type));
        Ok(typ)
    }
    pub fn builtins(&mut self, b: uir::Builtinkind, l: Provenance) -> Type {
        let int_t = Type::simple(self.typevec, Typekind::Int, l);
        let bool_t = Type::simple(self.typevec, Typekind::Bool, l);
        let string_t = Type::simple(self.typevec, Typekind::String, l);
        let ints = [
            int_t.clone(),
            int_t.clone(),
            int_t.clone(),
            int_t.clone(),
            int_t.clone(),
            int_t.clone(),
            int_t.clone(),
        ];
        let int2_t = Type::tup(self.typevec, &ints[0..2], l);
        let int7_t = Type::tup(self.typevec, &ints, l);
        match b {
            uir::Builtinkind::Add
            | uir::Builtinkind::Sub
            | uir::Builtinkind::Mul
            | uir::Builtinkind::Div
            | uir::Builtinkind::Mod
            | uir::Builtinkind::And
            | uir::Builtinkind::Or
            | uir::Builtinkind::Xor
            | uir::Builtinkind::ShLeft
            | uir::Builtinkind::ShRight => Type::func(self.typevec, &int2_t, &int_t, l),
            uir::Builtinkind::Not | uir::Builtinkind::Negate => {
                Type::func(self.typevec, &int_t, &int_t, l)
            }
            uir::Builtinkind::CmpEq
            | uir::Builtinkind::CmpNE
            | uir::Builtinkind::CmpGt
            | uir::Builtinkind::CmpLt
            | uir::Builtinkind::CmpGE
            | uir::Builtinkind::CmpLE => Type::func(self.typevec, &int2_t, &bool_t, l),
            uir::Builtinkind::Syscall => Type::func(self.typevec, &int7_t, &int_t, l),
            uir::Builtinkind::PtrToInt => Type::func(self.typevec, &string_t, &int_t, l),
            uir::Builtinkind::IntToPtr => Type::func(self.typevec, &int_t, &string_t, l),
        }
    }
}

fn solve_constraints(
    typevec: &mut Ivec<uir::TypeId, Typekind>,
    modules: &Ivec<super::ModuleId, super::Module>,
    mut cs: Vec<Constraint>,
) -> Map<usize, TypeId> {
    let mut out = Map::new();
    while let Some(Constraint::Unify(a, b)) = cs.pop() {
        match (&typevec[a.kind], &typevec[b.kind]) {
            (Typekind::Var(a), _) => {
                out.insert(*a, b.kind);
                substitute(&mut cs, typevec, &out);
            }
            (_, Typekind::Var(b)) => {
                out.insert(*b, a.kind);
                substitute(&mut cs, typevec, &out);
            }
            (Typekind::Bundle(ks_a), Typekind::Bundle(ks_b)) => {
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
            (a_k, b_k) if a_k == b_k => {}
            (a_k, b_k) if a_k != b_k => {
                report(
                    modules,
                    Message::error("Could not unify types!", None),
                    &[
                        Message::note(
                            &format!("Type '{}' originates here:", a.fmt(typevec)),
                            Some(a.loc),
                        ),
                        Message::note(
                            &format!("Type '{}' originates here:", b.fmt(typevec)),
                            Some(b.loc),
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
