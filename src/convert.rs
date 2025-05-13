use std::{cell::OnceCell, collections::BTreeMap};

use crate::{
    control::{self, Query},
    data::{
        ast,
        files::{self, Files},
        quir::{self, *},
        Ident, Quir,
    },
    misc::Ivec,
    parse, Provenance,
};

pub type Map = control::Qmap<files::Id, quir::TypedeclId, Quir>;
pub type Data = Quir;
impl Query<files::Id, quir::TypedeclId> for Quir {
    type Inputs<'a> = (&'a Files, &'a mut parse::Map, &'a mut Quir, TypedeclId);

    fn query(
        map: &mut control::Qmap<files::Id, quir::TypedeclId, Self>,
        &file: &files::Id,
        (files, parsemap, quir, parent): Self::Inputs<'_>,
    ) -> quir::TypedeclId {
        let ast = parsemap.query(file, (files,));
        let name = files[file]
            .path
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned()
            .leak();
        let id = quir.items.typedecls.push(Typedecl {
            loc: Provenance::None,
            items: BTreeMap::new(),
            cases: Vec::new(),
            inherits: false,
            imports: ast.imports.iter().map(|i| i.convert()).collect(),
            name: Ident {
                loc: Provenance::None,
                name,
            },
            parent,
        });
        let mut items = BTreeMap::new();
        for &c in &files[file].children {
            let child = map.query(c, (files, parsemap, quir, id));
            let name = quir.items.typedecls[child].name.name;
            items.insert(name, Id::Typedecl(child));
        }
        for t in ast.types.iter() {
            let name = t.name.name;
            let t = t.convert(id, file, quir);
            items.insert(name, Id::Typedecl(t));
        }
        for f in ast.funcs.iter() {
            let name = f.name.name;
            let f = f.convert(file, quir);
            items.insert(name, Id::Function(f));
        }
        quir.items.typedecls[id].items = items;
        id
    }
}

impl ast::Import {
    fn convert(&self) -> quir::Import {
        quir::Import {
            loc: self.loc,
            path: self.path.clone(),
            target: OnceCell::new(),
        }
    }
}

impl ast::Typedecl {
    fn convert(&self, parent: TypedeclId, file: files::Id, data: &mut Quir) -> TypedeclId {
        let &ast::Typedecl {
            loc,
            name,
            ref cases,
        } = self;
        let id = data.items.typedecls.push(Typedecl {
            items: BTreeMap::new(),
            imports: Vec::new(),
            cases: Vec::new(),
            inherits: true,
            parent,
            loc,
            name,
        });
        let cases: Vec<_> = cases
            .iter()
            .enumerate()
            .map(|(i, c)| c.convert(file, id, i, data))
            .collect();
        for &c in cases.iter() {
            let name = data.items.typecases[c].name.name;
            data.items.typedecls[id].items.insert(name, Id::Typecase(c));
        }
        data.items.typedecls[id].cases = cases;
        id
    }
}

impl ast::Typecase {
    fn convert(
        &self,
        file: files::Id,
        parent: TypedeclId,
        index: usize,
        data: &mut Quir,
    ) -> TypecaseId {
        let &ast::Typecase {
            loc,
            name,
            ref binding,
        } = self;
        let mut bindings = Default::default();
        let binding = binding.convert(file, false, unit_type, data, &mut bindings);

        data.items.typecases.push(Typecase {
            bindings,
            binding,
            parent,
            index,
            name,
            loc,
        })
    }
}

impl ast::Function {
    fn convert(&self, file: files::Id, data: &mut Quir) -> FunctionId {
        let &ast::Function { name, loc, .. } = self;
        let mut locals = Locals::default();
        let ret = self.ret.convert(file, data);
        let (binding, body) = {
            let bool_t = Type {
                loc: Provenance::None,
                kind: data.types.intern(types::Bool.into()),
            };

            let bind = self
                .args
                .convert(file, false, unit_type, data, &mut locals.bindings);
            let args = locals.expr.push(Expr {
                typ: locals.bindings[bind].typ,
                kind: expr::Arguments.into(),
                loc: self.args.loc,
            });
            let head = locals.expr.push(Expr {
                typ: bool_t,
                kind: expr::Let(bind, args).into(),
                loc: self.args.loc,
            });
            let block = self.body.convert(file, data, &mut locals);
            (
                locals.bindings[bind].typ,
                locals.expr.push(Expr {
                    typ: locals.expr[block].typ,
                    kind: expr::Bareblock(vec![head, block]).into(),
                    loc: locals.expr[block].loc,
                }),
            )
        };

        data.items.funcs.push(Function {
            locals,
            binding,
            body,
            ret,
            name,
            loc,
        })
    }
}

impl ast::Type {
    fn convert(&self, file: files::Id, data: &mut Quir) -> Type {
        let kind = 'kind: {
            match self.kind {
                ast::Typekind::Func { ref args, ref ret } => {
                    let args = args.convert(file, data).kind;
                    let ret = ret.convert(file, data).kind;
                    data.types.intern(types::Func { args, ret }.into())
                }
                ast::Typekind::Call { ref args, ref func } => {
                    let func = func.convert(file, data).kind;
                    let args = args.convert(file, data).kind;
                    data.types.intern(types::Call { args, func }.into())
                }
                ast::Typekind::Bundle(ref fields) => {
                    let fields = fields.iter().map(|t| t.convert(file, data).kind).collect();
                    data.types.intern(types::Bundle(fields).into())
                }
                ast::Typekind::Recall(ref name) => {
                    if name.len() == 1 {
                        match name[0].name {
                            "string" => break 'kind data.types.intern(types::String.into()),
                            "int" => break 'kind data.types.intern(types::Int.into()),
                            "bool" => break 'kind data.types.intern(types::Bool.into()),
                            "tuple" => break 'kind data.types.intern(types::Tup.into()),
                            _ => {}
                        }
                    }
                    data.types
                        .intern(types::Recall(name.clone(), file, OnceCell::new()).into())
                }
            }
        };
        let loc = self.loc;
        Type { kind, loc }
    }
}

fn new_typevar(quir: &mut Quir) -> types::Id {
    let var = quir.types.tvar_count;
    quir.types.tvar_count += 1;
    quir.types.intern(types::Var(var).into())
}

impl quir::Types {
    pub fn unit(&mut self) -> types::Id {
        let args = self.intern(types::Bundle(Vec::new()).into());
        let func = self.intern(types::Tup.into());
        self.intern(types::Call { args, func }.into())
    }
}

fn unit_type(quir: &mut Quir) -> types::Id {
    quir.types.unit()
}

impl ast::Binding {
    fn convert(
        &self,
        file: files::Id,
        mutable: bool,
        type_hole: fn(&mut Quir) -> types::Id,
        data: &mut Quir,
        bindings: &mut Ivec<quir::binding::Id, quir::Binding>,
    ) -> binding::Id {
        self.kind
            .convert(file, self.loc, mutable, type_hole, data, bindings)
    }
}

impl ast::binding::Empty {
    pub fn convert(
        &self,
        _: files::Id,
        loc: Provenance,
        _: bool,
        type_hole: fn(&mut Quir) -> types::Id,
        data: &mut Quir,
        bindings: &mut Ivec<quir::binding::Id, quir::Binding>,
    ) -> binding::Id {
        bindings.push(Binding {
            typ: Type {
                kind: type_hole(data),
                loc,
            },
            kind: binding::Empty.into(),
            loc,
        })
    }
}

impl ast::binding::Name {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        mutable: bool,
        type_hole: fn(&mut Quir) -> types::Id,
        data: &mut Quir,
        bindings: &mut Ivec<quir::binding::Id, quir::Binding>,
    ) -> binding::Id {
        let &ast::binding::Name(name, ref typ) = self;
        let typ = match typ {
            Some(ref t) => t.convert(file, data),
            None => Type {
                kind: type_hole(data),
                loc,
            },
        };
        bindings.push(Binding {
            kind: binding::Name(name, mutable).into(),
            typ,
            loc,
        })
    }
}

impl ast::binding::Tuple {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        mutable: bool,
        type_hole: fn(&mut Quir) -> types::Id,
        data: &mut Quir,
        bindings: &mut Ivec<quir::binding::Id, quir::Binding>,
    ) -> binding::Id {
        let (ts, bs) = self
            .0
            .iter()
            .map(|b| {
                let b = b.convert(file, mutable, type_hole, data, bindings);
                (bindings[b].typ.kind, b)
            })
            .unzip();
        let typ = {
            let args = data.types.intern(types::Bundle(ts).into());
            let func = data.types.intern(types::Tup.into());
            Type {
                kind: data.types.intern(types::Call { args, func }.into()),
                loc,
            }
        };
        bindings.push(Binding {
            kind: binding::Tuple(bs).into(),
            typ,
            loc,
        })
    }
}

impl ast::binding::Constructor {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        mutable: bool,
        type_hole: fn(&mut Quir) -> types::Id,
        data: &mut Quir,
        bindings: &mut Ivec<quir::binding::Id, quir::Binding>,
    ) -> binding::Id {
        let &ast::binding::Constructor {
            constructor_loc,
            ref constructor,
            ..
        } = self;
        let fields = self
            .fields
            .convert(file, mutable, type_hole, data, bindings);
        let constructor = constructor.clone();
        let type_path = {
            let mut tmp = constructor.clone();
            tmp.pop();
            tmp
        };
        let typ = Type {
            kind: data
                .types
                .intern(types::Recall(type_path, file, OnceCell::new()).into()),
            loc: constructor_loc,
        };
        bindings.push(Binding {
            kind: binding::Constructor {
                loc: constructor_loc,
                constructor,
                fields,
                id: OnceCell::new(),
            }
            .into(),
            typ,
            loc,
        })
    }
}

impl ast::Expr {
    fn convert(&self, file: files::Id, data: &mut Quir, locals: &mut Locals) -> expr::Id {
        self.kind.convert(file, self.loc, data, locals)
    }
}

impl ast::expr::If {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let cond = self.cond.convert(file, data, locals);
        let smash = self.smash.convert(file, data, locals);
        let pass = self.pass.convert(file, data, locals);
        let typ = locals.expr[smash].typ;
        locals.expr.push(Expr {
            kind: expr::If { cond, smash, pass }.into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Call {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let func = self.func.convert(file, data, locals);
        let args = self.args.convert(file, data, locals);
        let typ = Type {
            kind: new_typevar(data),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Call { func, args }.into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Tuple {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let (ts, es) = self
            .0
            .iter()
            .map(|e| {
                let e = e.convert(file, data, locals);
                (locals.expr[e].typ.kind, e)
            })
            .unzip();
        let typ = {
            let args = data.types.intern(types::Bundle(ts).into());
            let func = data.types.intern(types::Tup.into());
            Type {
                kind: data.types.intern(types::Call { args, func }.into()),
                loc,
            }
        };
        locals.expr.push(Expr {
            kind: expr::Tuple(es).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Loop {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let body = self.0.convert(file, data, locals);
        let typ = Type {
            kind: new_typevar(data),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Loop(body).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Bareblock {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let es = self
            .0
            .iter()
            .map(|e| e.convert(file, data, locals))
            .collect();
        let typ = Type {
            kind: new_typevar(data),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Bareblock(es).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Path {
    pub fn convert(
        &self,
        _: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let typ = Type {
            kind: new_typevar(data),
            loc,
        };
        if self.0.len() == 1 {
            for &v in quir::expr::Builtinkind::VALUES {
                if v.spelling() == self.0[0].name {
                    return locals.expr.push(Expr {
                        kind: v.into(),
                        typ,
                        loc,
                    });
                }
            }
        }
        locals.expr.push(Expr {
            kind: expr::Recall(self.0.clone(), OnceCell::new()).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Number {
    pub fn convert(
        &self,
        _: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let typ = Type {
            kind: data.types.intern(types::Int.into()),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Number(self.0).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::String {
    pub fn convert(
        &self,
        _: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let typ = Type {
            kind: data.types.intern(types::String.into()),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::String(self.0).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Bool {
    pub fn convert(
        &self,
        _: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let typ = Type {
            kind: data.types.intern(types::Bool.into()),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Bool(self.0).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Let {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let binding = self
            .0
            .convert(file, false, new_typevar, data, &mut locals.bindings);
        let expr = self.1.convert(file, data, locals);
        let typ = Type {
            kind: data.types.intern(types::Bool.into()),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Let(binding, expr).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Mut {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let binding = self
            .0
            .convert(file, true, new_typevar, data, &mut locals.bindings);
        let expr = self.1.convert(file, data, locals);
        let typ = Type {
            kind: data.types.intern(types::Bool.into()),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Let(binding, expr).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Assign {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let expr = self.1.convert(file, data, locals);
        let place = self.0.convert(file, data, locals);
        let typ = Type {
            kind: unit_type(data),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Assign(place, expr).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Break {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let typ = Type {
            kind: unit_type(data),
            loc,
        };
        let val = match self.0 {
            Some(ref val) => val.convert(file, data, locals),
            None => locals.expr.push(Expr {
                kind: expr::Tuple(Vec::new()).into(),
                typ,
                loc,
            }),
        };
        locals.expr.push(Expr {
            kind: expr::Break {
                val,
                target: OnceCell::new(),
            }
            .into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Return {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let typ = Type {
            kind: unit_type(data),
            loc,
        };
        let val = match self.0 {
            Some(ref val) => val.convert(file, data, locals),
            None => locals.expr.push(Expr {
                kind: expr::Tuple(Vec::new()).into(),
                typ,
                loc,
            }),
        };
        locals.expr.push(Expr {
            kind: expr::Return(val).into(),
            typ,
            loc,
        })
    }
}

impl ast::expr::Const {
    pub fn convert(
        &self,
        file: files::Id,
        loc: Provenance,
        data: &mut Quir,
        locals: &mut Locals,
    ) -> expr::Id {
        let inner = self.0.convert(file, data, locals);
        let typ = Type {
            kind: unit_type(data),
            loc,
        };
        locals.expr.push(Expr {
            kind: expr::Const(inner).into(),
            typ,
            loc,
        })
    }
}

// impl Quir {
//     pub fn intern(&mut self, kind: types::Kind) -> types::Id {
//         match self.typemap.get(&kind) {
//             Some(&id) => id,
//             None => {
//                 let id = self.types.push(kind.clone());
//                 self.typemap.insert(kind, id);
//                 id
//             }
//         }
//     }
// }
impl quir::Types {
    pub fn intern(&mut self, kind: types::Kind) -> types::Id {
        match self.typemap.get(&kind) {
            Some(&id) => id,
            None => {
                let id = self.types.push(kind.clone());
                self.typemap.insert(kind, id);
                id
            }
        }
    }
}
