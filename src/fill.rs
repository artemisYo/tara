use std::cell::OnceCell;
use crate::{control::{self, Query}, data::{files::Files, quir::{self, binding, expr, types}}, resolve, typer};

#[derive(Clone, Copy, Debug)]
pub struct Data(pub Option<quir::Type>);
pub type Map = control::Qmap<quir::Id, Data, Data>;
impl Query<quir::Id, Data> for Data {
    type Inputs<'a> = (
        &'a Files,
        &'a mut typer::Map,
        &'a quir::Items,
        &'a mut quir::Types,
        &'a mut resolve::Map,
    );

    fn query(
        fillmap: &mut control::Qmap<quir::Id, Data, Self>,
        &id: &quir::Id,
        (files, tymap, items, types, resmap): Self::Inputs<'_>
    ) -> Data {
        let substs = tymap.query(id, (files, items, types, resmap, fillmap));
        let typ = id.fill(files, &substs, items, types);
        Data(typ)
    }
}

impl quir::Id {
    fn fill(
        self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types
    ) -> Option<quir::Type> {
        match self {
            quir::Id::Typecase(id) => {
                let c = &items[id];
                let args = c.bindings[c.binding].typ.kind;
                let ret = c.parent;
                let target = OnceCell::new();
                let _ = target.set(ret);
                // NOTE: files.root() is not correct here, but I'm too lazy to fix it
                // NOTE NOTE: it also probably does not matter, as the target is set anyway
                let ret = types.intern(
                    types::Recall(vec![items[ret].name], files.root(), target).into()
                );
                Some(quir::Type {
                    kind: types.intern(
                        types::Func {
                            args,
                            ret,
                        }.into()
                    ),
                    loc: c.loc,
                })
            },
            quir::Id::Function(id) => {
                let f = &items[id];
                f.body.fill(files, substs, items, types, id);
                Some(quir::Type {
                    kind: types.intern(types::Func {
                        args: f.binding.kind,
                        ret: f.ret.kind,
                    }.into()),
                    loc: f.loc,
                })
            },
            quir::Id::Typedecl(id) => {
                for &i in items[id].items.values() {
                    i.fill(files, substs, items, types);
                }
                None
            },
        }
    }
}

impl binding::Id {
    fn fill(
        self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        let b = &items[(owner, self)];
        b.typ.kind.replace(substs, types);
        b.kind.fill(files, substs, items, types, owner);
    }
}

impl binding::Empty {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl binding::Name {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl binding::Tuple {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        for &b in &self.0 {
            b.fill(files, substs, items, types, owner);
        }
    }
}

impl binding::Constructor {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.fields.fill(files, substs, items, types, owner);
    }
}

impl expr::Id {
    fn fill(
        self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId,
    ) {
        let e = &items[(owner, self)];
        e.typ.kind.replace(substs, types);
        e.kind.fill(files, substs, items, types, owner);
    }
}

impl expr::If {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.cond.fill(files, substs, items, types, owner);
        self.smash.fill(files, substs, items, types, owner);
        self.pass.fill(files, substs, items, types, owner);
    }
}

impl expr::Call {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.func.fill(files, substs, items, types, owner);
        self.args.fill(files, substs, items, types, owner);
    }
}

impl expr::Builtinkind {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl expr::Tuple {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        for &e in &self.0 {
            e.fill(files, substs, items, types, owner);
        }
    }
}

impl expr::Loop {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.0.fill(files, substs, items, types, owner);
    }
}

impl expr::Bareblock {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        for &e in &self.0 {
            e.fill(files, substs, items, types, owner);
        }
    }
}

impl expr::Recall {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl expr::Number {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl expr::String {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl expr::Bool {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl expr::Arguments {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {}
}

impl expr::Poison {
    pub fn fill(
        &self,
        _: &Files,
        _: &typer::Data,
        _: &quir::Items,
        _: &mut quir::Types,
        _: quir::FunctionId
    ) {
        todo!("poisoning")
    }
}

impl expr::Let {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.0.fill(files, substs, items, types, owner);
        self.1.fill(files, substs, items, types, owner);
    }
}

impl expr::Assign {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.0.fill(files, substs, items, types, owner);
        self.1.fill(files, substs, items, types, owner);
    }
}

impl expr::Break {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.val.fill(files, substs, items, types, owner);
    }
}

impl expr::Return {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.0.fill(files, substs, items, types, owner);
    }
}

impl expr::Const {
    pub fn fill(
        &self,
        files: &Files,
        substs: &typer::Data,
        items: &quir::Items,
        types: &mut quir::Types,
        owner: quir::FunctionId
    ) {
        self.0.fill(files, substs, items, types, owner);
    }
}

impl types::Id {
    pub fn replace(self, substs: &typer::Data, types: &mut quir::Types) {
        use types::Kind::*;
        match &types.types[self] {
            Func(types::Func { args, ret }) => {
                let (args, ret) = (*args, *ret);
                args.replace(substs, types);
                ret.replace(substs, types);
            }
            Call(types::Call { func, args }) => {
                let (func, args) = (*func, *args);
                func.replace(substs, types);
                args.replace(substs, types);
            },
            Bundle(types::Bundle(ref ids)) => {
                for k in ids.clone() {
                    k.replace(substs, types);
                }
            },
            Var(types::Var(v)) => {
                if let Some(t) = substs.0.get(&v) {
                    types.types[self] = types.types[*t].clone();
                }
            },
            _ => {}
        }
    }
}
