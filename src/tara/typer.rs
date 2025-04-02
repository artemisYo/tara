use std::collections::BTreeMap as Map;
use std::rc::Rc;

use either::Either::{Left, Right};

use crate::{gen_query, message, misc::CheapClone, Provenance};

use super::{
    fill,
    quir::{self, expr, Binding, BindingId, Expr, ExprId, FunctionId, Type, TypeId, Typekind},
    resolve, Tara,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct In {
    pub i: quir::Id,
}

#[derive(Clone)]
pub struct Out {
    pub substitutions: Rc<Map<usize, TypeId>>,
}
impl CheapClone for Out {}

pub enum Constraint {
    Unify(u32, Type, Type),
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub(super) enum ControlFlow {
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

gen_query!(typecheck);
fn typecheck(tara: &mut Tara, In { i }: In) -> Out {
    let mut constraints = vec![];
    let module = tara.quir_items[i].module();
    tara.resolve(resolve::In { m: module });
    i.typecheck(&mut constraints, tara);
    let substitutions = solve_constraints(constraints, tara);
    Out {
        substitutions: Rc::new(substitutions),
    }
}

macro_rules! unify {
    ($cs:expr, $a:expr, $b:expr) => {
        ($cs).unify(line!(), $a, $b)
    };
}

impl quir::Id {
    fn typecheck(self, constraints: &mut Vec<Constraint>, tara: &mut Tara) {
        match tara.quir_items[self] {
            quir::Item::Typecase(_) | quir::Item::Typedecl(_) => {}
            quir::Item::Namespace(ref n) => {
                for &id in n.items.cheap().values() {
                    id.typecheck(constraints, tara);
                }
            }
            quir::Item::Function(ref f) => {
                let body = f.body;
                let Typekind::Func { ret, .. } = tara.quir_types[f.typ.kind] else {
                    panic!();
                };
                let ret = Type {
                    loc: f.typ.loc,
                    kind: ret,
                };
                let mut ctx = Ctx {
                    owner: self.into_func(),
                    constraints,
                    tara,
                };
                if let Ok(t) = body.typecheck(&mut ctx) {
                    //ctx.unify(ret, t);
                    unify!(ctx, ret, t);
                }
            }
        }
    }
}

pub(super) struct Ctx<'a> {
    owner: FunctionId,
    tara: &'a mut Tara,
    constraints: &'a mut Vec<Constraint>,
}
impl Ctx<'_> {
    fn unify(&mut self, line: u32, a: Type, b: Type) {
        self.constraints.push(Constraint::Unify(line, a, b));
    }
}
impl std::ops::Index<ExprId> for Ctx<'_> {
    type Output = Expr;
    fn index(&self, id: ExprId) -> &Self::Output {
        &self.tara.quir_items[self.owner].locals[id]
    }
}
impl std::ops::Index<BindingId> for Ctx<'_> {
    type Output = Binding;
    fn index(&self, id: BindingId) -> &Self::Output {
        &self.tara.quir_items[self.owner].locals[id]
    }
}

impl ExprId {
    fn typecheck(self, ctx: &mut Ctx) -> Result<Type, ControlFlow> {
        let e = ctx[self].cheap();
        let typ = e.kind.typecheck(e.loc, e.typ, ctx)?;
        // probably not needed like this, but here for sanity
        // ctx.unify(typ, e.typ);
        unify!(ctx, typ, e.typ);
        Ok(typ)
    }
}

impl expr::If {
    pub(super) fn typecheck(
        &self,
        _: Provenance,
        typ: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let cond_t = self.cond.typecheck(ctx)?;
        let cond_loc = ctx[self.cond].loc;
        let bool_t = Type {
            kind: ctx.tara.intern(Typekind::Bool),
            loc: cond_loc,
        };
        // ctx.unify(cond_t, bool_t);
        unify!(ctx, cond_t, bool_t);
        let smash = self.smash.typecheck(ctx);
        let pass = self.pass.typecheck(ctx);
        let (smash, pass) = match (smash, pass) {
            (Err(s), Err(p)) => return Err(s.minimum(p)),
            (Err(_), Ok(k)) | (Ok(k), Err(_)) => (k, typ),
            (Ok(s), Ok(p)) => (s, p),
        };
        // ctx.unify(smash, pass);
        unify!(ctx, smash, pass);
        Ok(smash)
    }
}

impl expr::Call {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        typ: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let func = self.func.typecheck(ctx)?;
        let args = self.args.typecheck(ctx)?.kind;
        let ret = typ.kind;
        let unapp = Type {
            kind: ctx.tara.intern(Typekind::Func { args, ret }),
            loc,
        };
        // ctx.unify(func, unapp);
        unify!(ctx, func, unapp);
        Ok(typ)
    }
}

impl expr::Builtinkind {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let int_k = ctx.tara.intern(Typekind::Int);
        let bool_k = ctx.tara.intern(Typekind::Bool);
        let string_k = ctx.tara.intern(Typekind::String);
        let int2_k = ctx.tara.tuple(Rc::new([int_k; 2]));
        let int7_k = ctx.tara.tuple(Rc::new([int_k; 7]));
        let mut func = |args, ret| Type {
            kind: ctx.tara.intern(Typekind::Func { args, ret }),
            loc,
        };
        Ok(match self {
            expr::Builtinkind::Add
            | expr::Builtinkind::Sub
            | expr::Builtinkind::Mul
            | expr::Builtinkind::Div
            | expr::Builtinkind::Mod
            | expr::Builtinkind::And
            | expr::Builtinkind::Or
            | expr::Builtinkind::Xor
            | expr::Builtinkind::ShLeft
            | expr::Builtinkind::ShRight => func(int2_k, int_k),
            expr::Builtinkind::Not | expr::Builtinkind::Negate => func(int_k, int_k),
            expr::Builtinkind::CmpEq
            | expr::Builtinkind::CmpNE
            | expr::Builtinkind::CmpGt
            | expr::Builtinkind::CmpLt
            | expr::Builtinkind::CmpGE
            | expr::Builtinkind::CmpLE => func(int2_k, bool_k),
            expr::Builtinkind::Syscall => func(int7_k, int_k),
            expr::Builtinkind::PtrToInt => func(string_k, int_k),
            expr::Builtinkind::IntToPtr => func(int_k, string_k),
        })
    }
}

impl expr::Tuple {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let mut fields = Vec::with_capacity(self.0.len());
        for &e in self.0.cheap().iter() {
            fields.push(e.typecheck(ctx)?.kind);
        }
        Ok(Type {
            kind: ctx.tara.tuple(fields.into()),
            loc,
        })
    }
}

impl expr::Loop {
    pub(super) fn typecheck(
        &self,
        _: Provenance,
        typ: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        if Err(ControlFlow::Return) == self.0.typecheck(ctx) {
            return Err(ControlFlow::Return);
        }
        Ok(typ)
    }
}

impl expr::Bareblock {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let mut t = Type {
            kind: ctx.tara.unit(),
            loc,
        };
        for &e in self.0.cheap().iter() {
            t = e.typecheck(ctx)?;
        }
        Ok(t)
    }
}

impl expr::Recall {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        match self.1.expect("resolution shoulda done this") {
            Left(bid) => Ok(ctx[bid].typ),
            Right(id) if id == ctx.owner.into() => Ok(ctx.tara.quir_items[ctx.owner].typ),
            Right(id) => Ok(match ctx.tara.fill(fill::In { i: id }).typ {
                Some(t) => t,
                None => {
                    ctx.tara.report(
                        message!(
                            error @ loc
                                => "Expected a term, but name '{}' does not refer to one!", self.0.last().unwrap().name.0,
                        ),
                        &[message!(
                            note @ ctx.tara.quir_items[id].loc()
                                => "Name declared here!"
                        )],
                        );
                    todo!("poisoning")
                }
            }),
            // Right(id) => Ok(match ctx.tara.quir_items[id] {
            //     quir::Item::Typecase(ref c) => c.typ,
            //     quir::Item::Function(ref f) => f.typ,
            //     quir::Item::Typedecl(ref t) => {
            //         ctx.tara.report(
            //             message!(
            //                 error @ loc
            //                     => "Name '{}' refers to a type, but a term was expected!", t.name.name.0
            //             ),
            //             &[
            //                 message!(note @ t.loc => "Type defined here!")
            //             ]
            //         );
            //         todo!("poisoning")
            //     }
            //     quir::Item::Namespace(_) => {
            //         ctx.tara.report(
            //             message!(
            //                 error @ loc
            //                     => "The following path refers to a namespace, while a term was expected!"
            //             ),
            //             &[
            //                 message!(note => "Was a part of that path lost?"),
            //             ]
            //         );
            //         todo!("poisoning")
            //     }
            // }),
        }
    }
}

impl expr::Number {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        Ok(Type {
            kind: ctx.tara.intern(Typekind::Int),
            loc,
        })
    }
}

impl expr::String {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        Ok(Type {
            kind: ctx.tara.intern(Typekind::String),
            loc,
        })
    }
}

impl expr::Bool {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        Ok(Type {
            kind: ctx.tara.intern(Typekind::Bool),
            loc,
        })
    }
}

impl expr::Arguments {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let t = ctx.tara.quir_items[ctx.owner].typ.kind;
        let Typekind::Func { args: t, .. } = ctx.tara.quir_types[t] else {
            panic!("expr::Arguments outside of a function??")
        };
        Ok(Type { kind: t, loc })
    }
}

impl expr::Poison {
    pub(super) fn typecheck(
        &self,
        _: Provenance,
        _: Type,
        _: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        std::process::exit(1);
    }
}

impl expr::Let {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let ty = self.1.typecheck(ctx)?;
        // ctx.unify(ty, ctx[self.0].typ);
        unify!(ctx, ty, ctx[self.0].typ);
        Ok(Type {
            kind: ctx.tara.unit(),
            loc,
        })
    }
}

impl expr::Assign {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let bid = self.1.expect("resolve should've done this");
        let quir::binding::Kind::Name(n) = ctx[bid].kind else {
            panic!("this shouldn't be parseable yet!")
        };
        if !n.1 {
            ctx.tara.report(
                message!(error @ loc => "Cannot reassign an immutable variable!"),
                &[message!( note @ ctx[bid].loc => "'{}' is declared here!", n.0.0)],
            );
            todo!("poisoning")
        }
        let ty = self.2.typecheck(ctx)?;
        // ctx.unify(ty, ctx[bid].typ);
        unify!(ctx, ty, ctx[bid].typ);
        Ok(Type {
            kind: ctx.tara.unit(),
            loc,
        })
    }
}

impl expr::Break {
    pub(super) fn typecheck(
        &self,
        _: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let target = self.target.expect("resolve should've done this");
        let ty = self.val.typecheck(ctx)?;
        // ctx.unify(ty, ctx[target].typ);
        unify!(ctx, ty, ctx[target].typ);
        Err(ControlFlow::Break)
    }
}

impl expr::Return {
    pub(super) fn typecheck(
        &self,
        _: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        let ret = ctx.tara.quir_items[ctx.owner].typ.kind;
        let Typekind::Func { ret, .. } = ctx.tara.quir_types[ret] else {
            panic!("non-func-typed function?")
        };
        let ret = Type {
            loc: ctx.tara.quir_items[ctx.owner].typ.loc,
            kind: ret,
        };
        let ty = self.0.typecheck(ctx)?;
        // ctx.unify(ret, ty);
        unify!(ctx, ret, ty);
        Err(ControlFlow::Return)
    }
}

impl expr::Const {
    pub(super) fn typecheck(
        &self,
        loc: Provenance,
        _: Type,
        ctx: &mut Ctx,
    ) -> Result<Type, ControlFlow> {
        self.0.typecheck(ctx)?;
        Ok(Type {
            kind: ctx.tara.unit(),
            loc,
        })
    }
}

fn solve_constraints(mut cs: Vec<Constraint>, tara: &mut Tara) -> Map<usize, TypeId> {
    let mut out = Map::new();
    while let Some(Constraint::Unify(line_nr, a, b)) = cs.pop() {
        match (&tara.quir_types[a.kind], &tara.quir_types[b.kind]) {
            (Typekind::Var(a), _) => {
                out.insert(*a, b.kind);
                substitute(&mut cs, &out, tara);
            }
            (_, Typekind::Var(b)) => {
                out.insert(*b, a.kind);
                substitute(&mut cs, &out, tara);
            }
            (Typekind::Bundle(ref ks_a), Typekind::Bundle(ref ks_b)) => {
                for (&k_a, &k_b) in ks_a.clone().iter().zip(ks_b.clone().iter()) {
                    cs.push(Constraint::Unify(
                        line_nr,
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
                    line_nr,
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
                    line_nr,
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
                    line_nr,
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
                    line_nr,
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
                dbg!(&tara.quir_types[a.kind], &tara.quir_types[b.kind]);
                println!("CODE #{}", line_nr);
                tara.report(
                    message!(error => "Could not unify types!"),
                    &[
                        message!(
                            note @ a.loc =>
                            "Type '{}' originates here:", a.fmt(tara)
                        ),
                        message!(
                            note @ b.loc =>
                            "Type '{}' originates here:", b.fmt(tara)
                        ),
                    ],
                );
                todo!("poisoning or smt")
            }
            _ => unreachable!(),
        }
    }
    out
}

fn substitute(cs: &mut [Constraint], map: &Map<usize, TypeId>, tara: &mut Tara) {
    for c in cs.iter_mut() {
        match c {
            Constraint::Unify(_, a, b) => {
                tara.replace(a.kind, map);
                tara.replace(b.kind, map);
            }
        }
    }
}

impl Tara {
    pub fn replace(&mut self, id: TypeId, map: &Map<usize, TypeId>) {
        match self.quir_types[id] {
            Typekind::Func { args, ret } => {
                self.replace(args, map);
                self.replace(ret, map);
            }
            Typekind::Call { args, func } => {
                self.replace(func, map);
                self.replace(args, map);
            }
            Typekind::Bundle(ref ids) => {
                for &k in ids.cheap().iter() {
                    self.replace(k, map);
                }
            }
            Typekind::Var(v) => {
                if let Some(t) = map.get(&v) {
                    self.quir_types[id] = self.quir_types[*t].cheap();
                }
            }
            _ => {}
        }
    }
}
