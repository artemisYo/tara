use either::Either::{Left, Right};
use std::collections::HashMap;
use crate::{
    control::{self, Query}, data::{files::Files, quir, Quir}, message, misc::Ivec, Provenance
};

pub struct Data;
pub type Map = control::Qmap<quir::TypedeclId, (), Data>;
impl Query<quir::TypedeclId, ()> for Data {
    type Inputs<'a> = (&'a Files, &'a Quir,);

    fn query(
        map: &mut Map,
        &module: &quir::TypedeclId,
        (files, quir,): Self::Inputs<'_>
    ) {
        let mut ctx = Ctx::default();
        assert!(
            !quir.items[module].inherits,
            "resolution has to start in the context of some file"
        );
        module.resolve(map, files, &mut ctx, quir);
    }
}

#[derive(Debug, Clone, Copy)]
enum AnyId {
    Global(quir::Id),
    Local(quir::LocalId),
}
impl AnyId {
    fn kind(self) -> &'static str {
        match self {
            AnyId::Global(g) => g.kind(),
            AnyId::Local(quir::LocalId::Expr(_)) => "expression",
            AnyId::Local(quir::LocalId::Binding(_)) => "binding",
        }
    }
}
impl quir::Id {
    fn kind(self) -> &'static str {
        match self {
            quir::Id::Typecase(_) => "constructor",
            quir::Id::Typedecl(_) => "type",
            quir::Id::Function(_) => "function",
        }
    }
}

#[derive(Default)]
pub struct Ctx<'a> {
    parent: Option<&'a Ctx<'a>>,
    names: HashMap<&'static str, AnyId>,
    break_target: Option<quir::expr::Id>,
}
impl Ctx<'_> {
    fn get(&self, name: &str) -> Option<AnyId> {
        if let Some(&id) = self.names.get(name) {
            return Some(id);
        }
        self.parent.and_then(|p| p.get(name))
    }
    fn break_target(&self) -> Result<quir::expr::Id, ()> {
        if let Some(id) = self.break_target {
            return Ok(id);
        }
        self.parent.ok_or(()).and_then(|p| p.break_target())
    }
    fn sub(&self, break_target: Option<quir::expr::Id>) -> Ctx<'_> {
        Ctx {
            parent: Some(self),
            names: Default::default(),
            break_target,
        }
    }
}

fn resolve_import(
    files: &Files,
    quir: &Quir,
    import: &quir::Import,
    mut head: quir::Id,
    ctx: &mut Ctx,
) -> Option<quir::Id> {
    let no_decl = |loc, head: quir::Id| -> ! {
        files.report(
            message!(error @? loc => "Cannot import from a {}!", head.kind()),
            &[]
        );
        todo!("exits");
    };
    if import.path.len() < 2 {
        files.report(
            message!(note @ import.loc => "Pointless import!"),
            &[message!(note => "Remove the import!")]
        );
        return None;
    }
    let mut head_loc = None;
    for &p in &import.path {
        if p.name == "..." {
            break;
        }
        let quir::Id::Typedecl(id) = head else {
            no_decl(head_loc, head);
        };
        head_loc = Some(p.loc);
        // NOTE: only works as no reexports are yet possible
        head = match id.index(quir, p.name) {
            Some(id) => id,
            None => {
                files.report(
                    message!(error @ p.loc => "Could not resolve name!"),
                    &[]
                );
                todo!("exits");
            },
        };
    }
    let name = import.path.last().unwrap().name;
    if name == "..." {
        let quir::Id::Typedecl(id) = head else {
            no_decl(head_loc, head);
        };
        for (&name, &id) in &quir.items[id].items {
            ctx.names.insert(name, AnyId::Global(id));
        }
    } else {
        ctx.names.insert(name, AnyId::Global(head));
    }
    Some(head)
}

impl quir::TypedeclId {
    fn resolve(self, map: &mut Map, files: &Files, ctx: &mut Ctx, quir: &Quir) {
        for i in &quir.items[self].imports {
            let Some(id) = resolve_import(
                files,
                quir,
                i,
                quir::Id::Typedecl(self),
                ctx
            ) else { break };
            let _ = i.target.set(id);
        }
        for (&name, &id) in &quir.items[self].items {
            ctx.names.insert(name, AnyId::Global(id));
        }

        for &id in quir.items[self].items.values() {
            id.resolve(map, files, ctx, quir);
        }
    }
}

impl quir::TypedeclId {
    // TODO: only works as expected because no reexports are yet possible
    pub fn index(self, quir: &Quir, name: &str) -> Option<quir::Id> {
        quir.items[self].items.get(name).copied()
    }
}

impl quir::Id {
    pub fn loc(self, quir: &Quir) -> Provenance {
        match self {
            quir::Id::Typedecl(id) => quir.items[id].loc,
            quir::Id::Typecase(id) => quir.items[id].loc,
            quir::Id::Function(id) => quir.items[id].loc,
        }
    }

    fn resolve(self, map: &mut Map, files: &Files, ctx: &mut Ctx, quir: &Quir) {
        match self {
            quir::Id::Typedecl(id) => {
                if quir.items[id].inherits {
                    id.resolve(map, files, &mut ctx.sub(None), quir);
                } else {
                    map.query(id, (files, quir));
                }
            },
            quir::Id::Typecase(id) => id.resolve(files, ctx, quir),
            quir::Id::Function(id) => id.resolve(files, ctx, quir),
        }
    }
}

impl quir::TypecaseId {
    fn resolve(self, files: &Files, ctx: &Ctx, quir: &Quir) {
        let c = &quir.items[self];
        c.binding.resolve(files, ctx, quir, &c.bindings);
    }
}

impl quir::FunctionId {
    fn resolve(self, files: &Files, ctx: &Ctx, quir: &Quir) {
        let func = &quir.items[self];
        func.ret.resolve(files, ctx, quir);
        func.binding.resolve(files, ctx, quir);
        func.body.resolve(
            files,
            &mut ctx.sub(None),
            quir,
            &func.locals,
        );
    }
}

impl quir::binding::Id {
    fn resolve(
        self,
        files: &Files,
        ctx: &Ctx,
        quir: &Quir,
        binds: &Ivec<quir::binding::Id, quir::Binding>
    ) {
        let b = &binds[self];
        b.typ.resolve(files, ctx, quir);
        b.kind.resolve(files, ctx, quir, binds);
    }

    fn register(
        self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        binds: &Ivec<quir::binding::Id, quir::Binding>
    ) {
        let b = &binds[self];
        b.kind.register(files, ctx, quir, binds, self);
    }
}

impl quir::binding::Empty {
    pub fn resolve(
        &self,
        _: &Files,
        _: &Ctx,
        _: &Quir,
        _: &Ivec<quir::binding::Id, quir::Binding>
    ) {}
    pub fn register(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &Ivec<quir::binding::Id, quir::Binding>,
        _: quir::binding::Id
    ) {}
}

impl quir::binding::Name {
    pub fn resolve(
        &self,
        _: &Files,
        _: &Ctx,
        _: &Quir,
        _: &Ivec<quir::binding::Id, quir::Binding>,
    ) {}

    pub fn register(
        &self,
        _: &Files,
        ctx: &mut Ctx,
        _: &Quir,
        _: &Ivec<quir::binding::Id, quir::Binding>,
        id: quir::binding::Id
    ) {
        ctx.names.insert(self.0, AnyId::Local(quir::LocalId::Binding(id)));
    }
}

impl quir::binding::Tuple {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &Ctx,
        quir: &Quir,
        binds: &Ivec<quir::binding::Id, quir::Binding>
    ) {
        for &f in &self.0 {
            f.resolve(files, ctx, quir, binds);
        }
    }

    pub fn register(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        binds: &Ivec<quir::binding::Id, quir::Binding>,
        _: quir::binding::Id
    ) {
        for &id in &self.0 {
            id.register(files, ctx, quir, binds);
        }
    }
}

impl quir::binding::Constructor {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &Ctx,
        quir: &Quir,
        binds: &Ivec<quir::binding::Id, quir::Binding>
    ) {
        if self.id.get().is_some() { return }
        self.fields.resolve(files, ctx, quir, binds);
        let mut head = match ctx.get(self.constructor[0].name) {
            Some(id) => id,
            None => {
                files.report(
                    message!(
                        error @ self.constructor[0].loc
                            => "Could not resolve name!"
                    ),
                    &[]
                );
                todo!("exits");
            }
        };
        for n in &self.constructor[1..] {
            let AnyId::Global(quir::Id::Typedecl(id)) = head else {
                files.report(
                    message!(error @ n.loc => "Cannot import from a {}!", head.kind()),
                    &[]
                );
                todo!("exits");
            };
            head = match id.index(quir, n.name) {
                Some(id) => AnyId::Global(id),
                None => {
                    files.report(
                        message!(error @ n.loc => "Could not resolve name!"),
                        &[]
                    );
                    todo!("exits");
                }
            };
        }
        let AnyId::Global(quir::Id::Typecase(id)) = head else {
            files.report(
                message!(
                    error @ self.loc
                        => "Expected a constructor, but got {}",
                            head.kind()
                ),
                &[]
            );
            todo!("exits");
        };
        let _ = self.id.set(id);
    }

    pub fn register(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        binds: &Ivec<quir::binding::Id, quir::Binding>,
        _: quir::binding::Id
    ) {
        self.fields.resolve(files, ctx, quir, binds)
    }
}

impl quir::expr::Id {
    fn resolve(
        self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
    ) {
        let e = &locals.expr[self];
        e.kind.resolve(files, ctx, quir, locals, e.loc, self);
    }
}

impl quir::expr::If {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        self.cond.resolve(files, ctx, quir, locals);
        self.smash.resolve(files, ctx, quir, locals);
        self.pass.resolve(files, ctx, quir, locals);
    }
}

impl quir::expr::Call {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        self.func.resolve(files, ctx, quir, locals);
        self.args.resolve(files, ctx, quir, locals);
    }
}

impl quir::expr::Builtinkind {
    pub fn resolve(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {}
}

impl quir::expr::Tuple {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        for &id in &self.0 {
            id.resolve(files, ctx, quir, locals);
        }
    }
}

impl quir::expr::Bareblock {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        let mut ctx = ctx.sub(None);
        for &id in &self.0 {
            id.resolve(files, &mut ctx, quir, locals);
        }
    }
}

impl quir::expr::Number {
    pub fn resolve(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {}
}

impl quir::expr::String {
    pub fn resolve(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {}
}

impl quir::expr::Bool {
    pub fn resolve(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {}
}

impl quir::expr::Arguments {
    pub fn resolve(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {}
}

impl quir::expr::Poison {
    pub fn resolve(
        &self,
        _: &Files,
        _: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {}
}

impl quir::expr::Let {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        self.0.resolve(files, ctx, quir, &locals.bindings);
        self.0.register(files, ctx, quir, &locals.bindings);
        self.1.resolve(files, ctx, quir, locals);
    }
}

impl quir::expr::Assign {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        self.0.resolve(files, ctx, quir, locals);
        self.1.resolve(files, ctx, quir, locals);
    }
}

impl quir::expr::Return {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        self.0.resolve(files, ctx, quir, locals);
    }
}

impl quir::expr::Const {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        _: quir::expr::Id,
    ) {
        self.0.resolve(files, ctx, quir, locals);
    }
}

impl quir::expr::Loop {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        locals: &quir::Locals,
        _: Provenance,
        id: quir::expr::Id,
    ) {
        self.0.resolve(
            files,
            &mut ctx.sub(Some(id)),
            quir,
            locals,
        );
    }
}

impl quir::expr::Break {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        _: &Quir,
        _: &quir::Locals,
        loc: Provenance,
        _: quir::expr::Id,
    ) {
        if self.target.get().is_some() { return }
        let target = match ctx.break_target() {
            Ok(id) => id,
            Err(()) => {
                files.report(
                    message!(
                        error @ loc
                            => "Break not contained by any loops"
                    ),
                    &[]
                );
                todo!("exits");
            }
        };
        let _ = self.target.set(target);
    }
}

impl quir::expr::Recall {
    pub fn resolve(
        &self,
        files: &Files,
        ctx: &mut Ctx,
        quir: &Quir,
        _: &quir::Locals,
        loc: Provenance,
        _: quir::expr::Id,
    ) {
        if self.1.get().is_some() { return }
        let mut head = match ctx.get(self.0[0].name) {
            Some(id) => id,
            None => {
                files.report(
                    message!(
                        error @ self.0[0].loc
                            => "Could not resolve name!"
                    ),
                    &[]
                );
                todo!("exits");
            },
        };
        for n in &self.0[1..] {
            let AnyId::Global(quir::Id::Typedecl(id)) = head else {
                files.report(
                    message!(error @ n.loc => "Cannot index a {}!", head.kind()),
                    &[]
                );
                todo!("exits");
            };
            head = match id.index(quir, n.name) {
                Some(id) => AnyId::Global(id),
                None => {
                    files.report(
                        message!(error @ n.loc => "Could not resolve name!"),
                        &[]
                    );
                    todo!("exits");
                }
            };
        }
        let id = match head {
            AnyId::Local(quir::LocalId::Binding(id)) => Left(id),
            AnyId::Local(quir::LocalId::Expr(_)) | AnyId::Global(quir::Id::Typedecl(_)) => {
                files.report(
                    message!(
                        error @ loc
                            => "Expected a term, but got {}",
                                head.kind()
                    ),
                    &[]
                );
                todo!("exits");
            },
            AnyId::Global(id) => Right(id),
        };
        let _ = self.1.set(id);
    }
}

impl quir::Type {
    fn resolve(self, files: &Files, ctx: &Ctx, quir: &Quir) {
        quir.types[self.kind].resolve(files, ctx, quir, self.loc);
    }
}

impl quir::types::Func {
    pub fn resolve(&self, files: &Files, ctx: &Ctx, quir: &Quir, loc: Provenance) {
        quir.types[self.args].resolve(files, ctx, quir, loc);
        quir.types[self.ret].resolve(files, ctx, quir, loc);
    }
}

impl quir::types::Call {
    pub fn resolve(&self, files: &Files, ctx: &Ctx, quir: &Quir, loc: Provenance) {
        quir.types[self.func].resolve(files, ctx, quir, loc);
        quir.types[self.args].resolve(files, ctx, quir, loc);
    }
}

impl quir::types::Bundle {
    pub fn resolve(&self, files: &Files, ctx: &Ctx, quir: &Quir, loc: Provenance) {
        for &id in &self.0 {
            quir.types[id].resolve(files, ctx, quir, loc);
        }
    }
}

impl quir::types::Var {
    pub fn resolve(&self, _: &Files, _: &Ctx, _: &Quir, _: Provenance) {}
}

impl quir::types::String {
    pub fn resolve(&self, _: &Files, _: &Ctx, _: &Quir, _: Provenance) {}
}

impl quir::types::Bool {
    pub fn resolve(&self, _: &Files, _: &Ctx, _: &Quir, _: Provenance) {}
}

impl quir::types::Int {
    pub fn resolve(&self, _: &Files, _: &Ctx, _: &Quir, _: Provenance) {}
}

impl quir::types::Tup {
    pub fn resolve(&self, _: &Files, _: &Ctx, _: &Quir, _: Provenance) {}
}

impl quir::types::Recall {
    pub fn resolve(&self, files: &Files, ctx: &Ctx, quir: &Quir, loc: Provenance) {
        if self.2.get().is_some() { return }
        let mut head = match ctx.get(self.0[0].name) {
            Some(id) => id,
            None => {
                dbg!(self.0[0].name);
                files.report(
                    message!(
                        error @ self.0[0].loc
                            => "Could not resolve name!"
                    ),
                    &[]
                );
                todo!("exits");
            },
        };
        for n in &self.0[1..] {
            let AnyId::Global(quir::Id::Typedecl(id)) = head else {
                files.report(
                    message!(error @ n.loc => "Cannot import from a {}!", head.kind()),
                    &[]
                );
                todo!("exits");
            };
            head = match id.index(quir, n.name) {
                Some(id) => AnyId::Global(id),
                None => {
                    files.report(
                        message!(error @ n.loc => "Could not resolve name!"),
                        &[]
                    );
                    todo!("exits");
                }
            };
        }
        let AnyId::Global(quir::Id::Typedecl(id)) = head else {
            files.report(
                message!(
                    error @ loc
                        => "Expected a type, but got {}",
                            head.kind()
                ),
                &[]
            );
            todo!("exits");
        };
        let _ = self.2.set(id);
    }
}

impl std::ops::Index<quir::TypedeclId> for quir::Items {
    type Output = quir::Typedecl;

    fn index(&self, id: quir::TypedeclId) -> &Self::Output {
        &self.typedecls[id]
    }
}
impl std::ops::IndexMut<quir::TypedeclId> for quir::Items {
    fn index_mut(&mut self, id: quir::TypedeclId) -> &mut Self::Output {
        &mut self.typedecls[id]
    }
}
impl std::ops::Index<quir::TypecaseId> for quir::Items {
    type Output = quir::Typecase;

    fn index(&self, id: quir::TypecaseId) -> &Self::Output {
        &self.typecases[id]
    }
}
impl std::ops::IndexMut<quir::TypecaseId> for quir::Items {
    fn index_mut(&mut self, id: quir::TypecaseId) -> &mut Self::Output {
        &mut self.typecases[id]
    }
}
impl std::ops::Index<quir::FunctionId> for quir::Items {
    type Output = quir::Function;

    fn index(&self, id: quir::FunctionId) -> &Self::Output {
        &self.funcs[id]
    }
}
impl std::ops::IndexMut<quir::FunctionId> for quir::Items {
    fn index_mut(&mut self, id: quir::FunctionId) -> &mut Self::Output {
        &mut self.funcs[id]
    }
}

impl std::ops::Index<quir::types::Id> for quir::Types {
    type Output = quir::types::Kind;
    fn index(&self, index: quir::types::Id) -> &Self::Output {
        &self.types[index]
    }
}
impl std::ops::IndexMut<quir::types::Id> for quir::Types {
    fn index_mut(&mut self, index: quir::types::Id) -> &mut Self::Output {
        &mut self.types[index]
    }
}
