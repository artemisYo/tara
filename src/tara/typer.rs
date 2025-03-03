use std::rc::Rc;

use crate::{report, Message};

use super::{
    uir::{self, Expr, Function, Type, Typekind},
    Tara,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct In {
    pub i: uir::Id,
}

#[derive(Clone)]
pub struct Out {
    pub substitutions: Rc<[(usize, Type)]>,
}

impl Tara {
    pub fn typeck(&mut self, i: In) -> Out {
        match self.typecheck.get(&i) {
            Some(Some(o)) => o.clone(),
            Some(None) => panic!("typeck entered a cycle!"),
            None => {
                self.typecheck.insert(i, None);
                let data = Context::default().typeck(self, i);
                self.typecheck.insert(i, Some(data));
                self.typecheck.get(&i).cloned().unwrap().unwrap()
            }
        }
    }
}

pub enum Constraint {
    Unify(Type, Type),
}

#[derive(Default)]
struct Context {
    constraints: Vec<Constraint>,
}

impl Context {
    fn typeck(mut self, ctx: &mut Tara, i: In) -> Out {
        let f = &ctx.uir_items[i.i];
        let body = self.expressions(ctx, f, &f.locals[f.body]);
        self.constraints.push(Constraint::Unify(
            f.typ.return_type().unwrap().clone(),
            body,
        ));
        let substitutions = solve_constraints(ctx, self.constraints);
        Out {
            substitutions: substitutions.into(),
        }
    }

    fn expressions(&mut self, ctx: &Tara, f: &Function, e: &Expr) -> Type {
        let typ = match e.kind {
            uir::Exprkind::Poison => e.typ.clone(),
            uir::Exprkind::If { cond, smash, pass } => {
                let cond_t = self.expressions(ctx, f, &f.locals[cond]);
                self.constraints.push(Constraint::Unify(
                    cond_t,
                    Type {
                        loc: f.locals[cond].loc,
                        kind: Typekind::Bool,
                    },
                ));
                let smash = self.expressions(ctx, f, &f.locals[smash]);
                let pass = self.expressions(ctx, f, &f.locals[pass]);
                self.constraints
                    .push(Constraint::Unify(smash.clone(), pass));
                smash
            }
            uir::Exprkind::Call { func, args } => {
                let func = self.expressions(ctx, f, &f.locals[func]);
                let args = self.expressions(ctx, f, &f.locals[args]);
                let ret = &e.typ;
                let unapp = Type {
                    loc: e.loc,
                    kind: Typekind::Func {
                        args: Box::new(args),
                        ret: Box::new(ret.clone()),
                    },
                };
                self.constraints.push(Constraint::Unify(func, unapp));
                ret.clone()
            }
            uir::Exprkind::Tuple(ref expr_ids) => Type::tup(
                expr_ids
                    .iter()
                    .map(|&e| self.expressions(ctx, f, &f.locals[e]))
                    .collect(),
                e.loc,
            ),
            uir::Exprkind::Loop(id) => {
                self.expressions(ctx, f, &f.locals[id]);
                e.typ.clone()
            }
            uir::Exprkind::Bareblock(ref expr_ids) => {
                expr_ids.iter().fold(Type::unit(e.loc), |_, &e| {
                    self.expressions(ctx, f, &f.locals[e])
                })
            }
            uir::Exprkind::Recall(Ok(binding_id)) => f.locals[binding_id].typ.clone(),
            uir::Exprkind::Recall(Err(item_id)) => ctx.uir_items[item_id].typ.clone(),
            uir::Exprkind::Number(_) => Type {
                loc: e.loc,
                kind: Typekind::Int,
            },
            uir::Exprkind::String(_) => Type {
                loc: e.loc,
                kind: Typekind::String,
            },
            uir::Exprkind::Bool(_) => Type {
                loc: e.loc,
                kind: Typekind::Bool,
            },
            uir::Exprkind::Arguments => e.typ.clone(),
            uir::Exprkind::Let(binding_id, expr_id) => {
                let bind = f.locals[binding_id].typ.clone();
                let expr = self.expressions(ctx, f, &f.locals[expr_id]);
                self.constraints.push(Constraint::Unify(bind, expr));
                Type::unit(e.loc)
            }
            uir::Exprkind::Assign(binding_id, expr_id) => {
                if let uir::Bindkind::Name(n, false) = f.locals[binding_id].kind {
                    report(
                        ctx,
                        Message::error("Cannot reassign an immutable variable!", Some(e.loc)),
                        &[Message::note(
                            &format!("'{}' is declared here!", n.0),
                            Some(f.locals[binding_id].loc),
                        )],
                    );
                }
                let bind = f.locals[binding_id].typ.clone();
                let expr = self.expressions(ctx, f, &f.locals[expr_id]);
                self.constraints.push(Constraint::Unify(bind, expr));
                Type::unit(e.loc)
            }
            uir::Exprkind::Break { val, target } => {
                let val = self.expressions(ctx, f, &f.locals[val]);
                let target = f.locals[target].typ.clone();
                self.constraints.push(Constraint::Unify(val, target));
                Type::unit(e.loc)
            }
            uir::Exprkind::Return(expr_id) => {
                let val = self.expressions(ctx, f, &f.locals[expr_id]);
                let target = f.typ.return_type().unwrap().clone();
                self.constraints.push(Constraint::Unify(val, target));
                Type::unit(e.loc)
            }
            uir::Exprkind::Const(expr_id) => {
                self.expressions(ctx, f, &f.locals[expr_id]);
                Type::unit(e.loc)
            }
        };
        // probably not always needed but included for sanity
        self.constraints
            .push(Constraint::Unify(typ.clone(), e.typ.clone()));
        typ
    }
}

fn solve_constraints(ctx: &Tara, mut cs: Vec<Constraint>) -> Vec<(usize, Type)> {
    let mut out = Vec::new();
    while let Some(c) = cs.pop() {
        match c {
            Constraint::Unify(a, b) if a == b => {}
            Constraint::Unify(
                Type {
                    kind: Typekind::Var(a),
                    ..
                },
                b,
            ) => {
                out.push((a, b));
                substitute(&mut cs, out.last().unwrap());
            }
            Constraint::Unify(
                a,
                Type {
                    kind: Typekind::Var(b),
                    ..
                },
            ) => {
                out.push((b, a));
                substitute(&mut cs, out.last().unwrap());
            }
            Constraint::Unify(
                Type {
                    kind: Typekind::Bundle(v),
                    ..
                },
                Type {
                    kind: Typekind::Bundle(w),
                    ..
                },
            ) => {
                for (a, b) in v.into_iter().zip(w.into_iter()) {
                    cs.push(Constraint::Unify(a, b))
                }
            }
            Constraint::Unify(
                Type {
                    kind: Typekind::Call { args: a1, func: f1 },
                    ..
                },
                Type {
                    kind: Typekind::Call { args: a2, func: f2 },
                    ..
                },
            ) => {
                cs.push(Constraint::Unify(*a1, *a2));
                cs.push(Constraint::Unify(*f1, *f2));
            }
            Constraint::Unify(
                Type {
                    kind: Typekind::Func { args: a1, ret: r1 },
                    ..
                },
                Type {
                    kind: Typekind::Func { args: a2, ret: r2 },
                    ..
                },
            ) => {
                cs.push(Constraint::Unify(*a1, *a2));
                cs.push(Constraint::Unify(*r1, *r2));
            }
            Constraint::Unify(a, b) if a != b => {
                report(
                    ctx,
                    Message::error("Could not unify types!", None),
                    &[
                        Message::note(&format!("Type '{}' originates here:", a), Some(a.loc)),
                        Message::note(&format!("Type '{}' originates here:", b), Some(b.loc))
                    ],
                );
                // TODO: poisoning or smt
                std::process::exit(1);
            }
            Constraint::Unify(a, b) => unreachable!("huh?\na: {a:#?}\nb: {b:#?}"),
        }
    }
    out
}

fn substitute(cs: &mut [Constraint], (t, by): &(usize, Type)) {
    for c in cs.iter_mut() {
        match c {
            Constraint::Unify(a, b) => {
                a.replace(*t, by);
                b.replace(*t, by);
            }
        }
    }
}
