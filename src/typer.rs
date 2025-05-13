use std::{collections::BTreeMap, rc::Rc};

use either::Either::{Left, Right};

use crate::{control::{self, Query}, data::{files::Files, quir::{self, expr, types, Type}}, fill, message, resolve, Provenance};


#[derive(Default, Hash, Debug)]
pub struct Data(pub BTreeMap<usize, quir::types::Id>);
pub type Map = control::Qmap<quir::Id, Rc<Data>, Data>;
impl Query<quir::Id, Rc<Data>> for Data {
    type Inputs<'a> = (
        &'a Files,
        &'a quir::Items,
        &'a mut quir::Types,
        &'a mut resolve::Map,
        &'a mut fill::Map,
    );

    fn query(
        map: &mut Map,
        &id: &quir::Id,
        (files, items, types, resmap, fillmap): Self::Inputs<'_>
    ) -> Rc<Data> {
        let mut constraints = Vec::new();
        id.typeck(files, fillmap, map, resmap, items, types, &mut constraints);
        let substitutions = solve(files, constraints, types);
        Rc::new(substitutions)
    }
}

#[derive(Debug)]
enum Constraint {
    Unify(u32, quir::Type, quir::Type),
}
type Cons = Vec<Constraint>;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ControlFlow {
    Break,
    Return,
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
pub type Result = std::result::Result<quir::Type, ControlFlow>;

impl quir::Id {
    fn typeck(
        self,
        files: &Files,
        fillmap: &mut fill::Map,
        tymap: &mut self::Map,
        resmap: &mut resolve::Map,
        items: &quir::Items,
        types: &mut quir::Types,
        cons: &mut Cons
    ) {
        match self {
            quir::Id::Typecase(_) => {},
            quir::Id::Typedecl(id) => items.typedecls[id].typeck(files, fillmap, tymap, resmap, items, types, cons),
            quir::Id::Function(id) => id.typeck(files, fillmap, tymap, resmap, items, types, cons),
        }
    }
}

impl quir::Typedecl {
    fn typeck(
        &self,
        files: &Files,
        fillmap: &mut fill::Map,
        tymap: &mut self::Map,
        resmap: &mut resolve::Map,
        items: &quir::Items,
        types: &mut quir::Types,
        cons: &mut Cons
    ) {
        for &id in self.imports.iter().filter_map(|i| i.target.get()) {
            id.typeck(files, fillmap, tymap, resmap, items, types, cons);
        }
        for &id in self.items.values() {
            id.typeck(files, fillmap, tymap, resmap, items, types, cons);
        }
    }
}

pub struct Ctx<'a> {
    owner: quir::FunctionId,
    items: &'a quir::Items,
    fillmap: &'a mut fill::Map,
    tymap: &'a mut self::Map,
    resmap: &'a mut resolve::Map,
    types: &'a mut quir::Types,
    files: &'a Files,
    cons: &'a mut Cons,
}
impl std::ops::Index<quir::expr::Id> for Ctx<'_> {
    type Output = quir::Expr;
    fn index(&self, id: quir::expr::Id) -> &Self::Output {
        &self.items[(self.owner, id)]
    }
}
impl std::ops::Index<quir::binding::Id> for Ctx<'_> {
    type Output = quir::Binding;
    fn index(&self, id: quir::binding::Id) -> &Self::Output {
        &self.items[(self.owner, id)]
    }
}

macro_rules! unify {
    ($cs:expr, $a:expr, $b:expr) => {
        ($cs).push(Constraint::Unify(line!(), $a, $b))
    };
}

impl quir::FunctionId {
    fn typeck(
        self,
        files: &Files,
        fillmap: &mut fill::Map,
        tymap: &mut self::Map,
        resmap: &mut resolve::Map,
        items: &quir::Items,
        types: &mut quir::Types,
        cons: &mut Cons
    ) {
        let mut ctx = Ctx {
            owner: self,
            files,
            fillmap,
            tymap,
            resmap,
            items,
            types,
            cons,
        };
        if let Ok(t) = items[self].body.typeck(&mut ctx) {
            // cons.push(Constraint::Unify(t, items[self].ret));
            unify!(cons, t, items[self].ret);
        }
    }
}

impl quir::expr::Id {
    fn typeck(self, ctx: &mut Ctx) -> Result {
        let e = &ctx.items[(ctx.owner, self)];
        let typ = e.kind.typeck(ctx, e.loc, e.typ)?;
        // probably not needed, but here for sanity
        // ctx.cons.push(Constraint::Unify(typ, e.typ));
        unify!(ctx.cons, typ, e.typ);
        Ok(typ)
    }
}

impl quir::expr::If {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        _: Provenance,
        typ: quir::Type
    ) -> Result {
        let cond = self.cond.typeck(ctx)?;
        let bool_t = Type {
            kind: ctx.types.intern(quir::types::Bool.into()),
            loc: Provenance::None,
        };
        // ctx.cons.push(Constraint::Unify(cond, bool_t));
        unify!(ctx.cons, cond, bool_t);
        let smash = self.smash.typeck(ctx);
        let pass = self.pass.typeck(ctx);
        let (smash, pass) = match (smash, pass) {
            (Err(s), Err(p)) => return Err(s.minimum(p)),
            (Err(_), Ok(k)) | (Ok(k), Err(_)) => (k, typ),
            (Ok(s), Ok(p)) => (s, p)
        };
        // ctx.cons.push(Constraint::Unify(smash, pass));
        unify!(ctx.cons, smash, pass);
        Ok(smash)
    }
}

impl expr::Call {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        typ: quir::Type
    ) -> Result {
        let func = self.func.typeck(ctx)?;
        let args = self.args.typeck(ctx)?;
        let ret = typ;
        let unapp = Type {
            kind: ctx.types.intern(types::Func {
                args: args.kind,
                ret: ret.kind
            }.into()),
            loc
        };
        // ctx.cons.push(Constraint::Unify(func, unapp));
        unify!(ctx.cons, func, unapp);
        Ok(typ)
    }
}

impl expr::Builtinkind {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        let int_k = ctx.types.intern(types::Int.into());
        let bool_k = ctx.types.intern(types::Bool.into());
        let string_k = ctx.types.intern(types::String.into());
        let int2_k = ctx.types.tuple(vec![int_k; 2]);
        let int7_k = ctx.types.tuple(vec![int_k; 7]);
        let mut func = |args, ret| Type {
            kind: ctx.types.intern(types::Func { args, ret }.into()),
            loc,
        };

        use expr::Builtinkind::*;
        Ok(match self {
            Add
            | Sub
            | Mul
            | Div
            | Mod
            | And
            | Or
            | Xor
            | ShLeft
            | ShRight => func(int2_k, int_k),
            Not | Negate => func(int_k, int_k),
            CmpEq
            | CmpNE
            | CmpGt
            | CmpLt
            | CmpGE
            | CmpLE => func(int2_k, bool_k),
            Syscall => func(int7_k, int_k),
            PtrToInt => func(string_k, int_k),
            IntToPtr => func(int_k, string_k),
        })
    }
}

impl quir::expr::Tuple {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        let mut fields = Vec::with_capacity(self.0.len());
        for &e in &self.0 {
            fields.push(e.typeck(ctx)?.kind);
        }
        Ok(Type {
            kind: ctx.types.tuple(fields),
            loc,
        })
    }
}

impl quir::expr::Loop {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        _: Provenance,
        typ: quir::Type
    ) -> Result {
        if Err(ControlFlow::Return) == self.0.typeck(ctx) {
            return Err(ControlFlow::Return);
        }
        Ok(typ)
    }
}

impl quir::expr::Bareblock {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        let mut t = Type {
            kind: ctx.types.unit(),
            loc
        };
        for &e in &self.0 {
            t = e.typeck(ctx)?;
        }
        Ok(t)
    }
}

impl expr::Recall {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        match self.1.get().copied().unwrap() {
            Left(bid) => Ok(ctx.items[(ctx.owner, bid)].typ),
            Right(id) if id == quir::Id::Function(ctx.owner) => {
                let f = &ctx.items[ctx.owner];
                let typ = Type {
                    kind: ctx.types.intern(types::Func {
                        args: f.binding.kind,
                        ret: f.ret.kind,
                    }.into()),
                    loc
                };
                Ok(typ)
            }
            Right(id) => match ctx.fillmap.query(
                id,
                (ctx.files, ctx.tymap, ctx.items, ctx.types, ctx.resmap)
            ).0 {
                Some(typ) => Ok(typ),
                None => {
                    ctx.files.report(
                        message!(
                            error @ loc
                                => "Expected a term, but the path does not refer to one!"
                        ),
                        &[]
                    );
                    todo!("exits");
                },
            },
        }
    }
}

impl expr::Number {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        Ok(Type {
            kind: ctx.types.intern(types::Int.into()),
            loc,
        })
    }
}

impl expr::String {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        Ok(Type {
            kind: ctx.types.intern(types::String.into()),
            loc,
        })
    }
}

impl expr::Bool {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        Ok(Type {
            kind: ctx.types.intern(types::Bool.into()),
            loc,
        })
    }
}

impl expr::Arguments {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        _: Provenance,
        _: quir::Type
    ) -> Result {
        Ok(ctx.items[ctx.owner].binding)
    }
}

impl expr::Poison {
    pub fn typeck(
        &self,
        _: &mut Ctx,
        _: Provenance,
        _: quir::Type
    ) -> Result {
        todo!("idk")
    }
}

impl expr::Let {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        let ty = self.1.typeck(ctx)?;
        let by = ctx.items[(ctx.owner, self.0)].typ;
        // ctx.cons.push(Constraint::Unify(ty, by));
        unify!(ctx.cons, ty, by);
        Ok(Type {
            kind: ctx.types.intern(types::Bool.into()),
            loc
        })
    }
}

impl expr::Assign {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        // NOTE: mutability checking does not exist this way
        let ly = self.0.typeck(ctx)?;
        let ry = self.1.typeck(ctx)?;
        // ctx.cons.push(Constraint::Unify(ly, ry));
        unify!(ctx.cons, ly, ry);
        Ok(Type {
            kind: ctx.types.unit(),
            loc
        })
    }
}

impl expr::Break {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        _: Provenance,
        _: quir::Type
    ) -> Result {
        let target = self.target.get().copied().unwrap();
        let target = ctx.items[(ctx.owner, target)].typ;
        let ty = self.val.typeck(ctx)?;
        // ctx.cons.push(Constraint::Unify(target, ty));
        unify!(ctx.cons, target, ty);
        Err(ControlFlow::Break)
    }
}

impl expr::Return {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        _: Provenance,
        _: quir::Type
    ) -> Result {
        let ret = ctx.items[ctx.owner].ret;
        let ty = self.0.typeck(ctx)?;
        // ctx.cons.push(Constraint::Unify(ret, ty));
        unify!(ctx.cons, ret, ty);
        Err(ControlFlow::Return)
    }
}

impl expr::Const {
    pub fn typeck(
        &self,
        ctx: &mut Ctx,
        loc: Provenance,
        _: quir::Type
    ) -> Result {
        self.0.typeck(ctx)?;
        Ok(Type {
            kind: ctx.types.unit(),
            loc
        })
    }
}

impl std::ops::Index<(quir::FunctionId, quir::expr::Id)> for quir::Items {
    type Output = quir::Expr;
    fn index(&self, (fid, id): (quir::FunctionId, quir::expr::Id)) -> &Self::Output {
        &self.funcs[fid].locals.expr[id]
    }
}
impl std::ops::Index<(quir::FunctionId, quir::binding::Id)> for quir::Items {
    type Output = quir::Binding;
    fn index(&self, (fid, id): (quir::FunctionId, quir::binding::Id)) -> &Self::Output {
        &self.funcs[fid].locals.bindings[id]
    }
}

impl quir::Types {
    fn tuple(&mut self, fields: Vec<types::Id>) -> types::Id {
        let bundle = self.intern(types::Bundle(fields).into());
        let tup = self.intern(types::Tup.into());
        self.intern(types::Call {
            func: tup,
            args: bundle,
        }.into())
    }
}

fn solve(files: &Files, mut cons: Cons, types: &mut quir::Types) -> Data {
    use types::Kind::*;
    let mut out = Data(BTreeMap::new());
    while let Some(Constraint::Unify(ln, a, b)) = cons.pop() {
        match (&types[a.kind], &types[b.kind]) {
            (Var(types::Var(a)), _) => {
                out.0.insert(*a, b.kind);
                substitute(&mut cons, &out, types);
            }
            (_, Var(types::Var(b))) => {
                out.0.insert(*b, a.kind);
                substitute(&mut cons, &out, types);
            }
            (Bundle(types::Bundle(ref ks_a)), Bundle(types::Bundle(ref ks_b))) => {
                for (&k_a, &k_b) in ks_a.iter().zip(ks_b.iter()) {
                    cons.push(Constraint::Unify(
                        ln,
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
            (Call(types::Call { args: a1, func: f1 }), Call(types::Call { args: a2, func: f2 })) => {
                cons.push(Constraint::Unify(
                    ln,
                    Type {
                        kind: *f1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *f2,
                        loc: b.loc,
                    }
                ));
                cons.push(Constraint::Unify(
                    ln,
                    Type {
                        kind: *a1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *a2,
                        loc: b.loc,
                    }
                ));
            }
            (Func(types::Func { args: a1, ret: r1 }), Func(types::Func { args: a2, ret: r2 })) => {
                cons.push(Constraint::Unify(
                    ln,
                    Type {
                        kind: *a1,
                        loc: a.loc,
                    },
                    Type {
                        kind: *a2,
                        loc: b.loc,
                    },
                ));
                cons.push(Constraint::Unify(
                    ln,
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
            // since the Eq derived impl cannot be changed
            // and takes the first two fields into account
            // we need to explicitly match the third field
            // to determine their equality.
            // Otherwise two names in different modules
            // referring to the same types, will be !=.
            (
                Recall(types::Recall(_, _, ref a_id)),
                Recall(types::Recall(_, _, ref b_id))
            ) if a_id == b_id => {},
            (ref a_k, ref b_k) if a_k == b_k => {}
            (ref a_k, ref b_k) if a_k != b_k => {
                dbg!(ln);
                dbg!(&a_k, &b_k);
                dbg!(a.fmt(&types.types), b.fmt(&types.types));
                files.report(
                    message!(error => "Could not unify types!"),
                    &[
                        message!(
                            note @ a.loc =>
                                "Type originates here:"
                        ),
                        message!(
                            note @ b.loc =>
                                "Type originates here:"
                        )
                    ],
                );
                // todo!("exits");
            }
            _ => unreachable!(),
        }
    }
    out
}

fn substitute(
    cons: &mut Cons,
    map: &Data,
    types: &mut quir::Types
) {
    for c in cons.iter_mut() {
        match c {
            Constraint::Unify(_, a, b) => {
                a.kind.replace(map, types);
                b.kind.replace(map, types);
            }
        }
    }
}

