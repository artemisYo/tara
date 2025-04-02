use std::{collections::BTreeMap as Map, rc::Rc};

use crate::{
    gen_query,
    misc::{CheapClone, Istr, Ivec},
    parse, CommonEnum, MkIndexer, Provenance,
};

use super::{parse::Ident, ModuleId, Tara};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct In {
    pub m: ModuleId,
}

#[derive(Clone, Copy)]
pub struct Out {
    pub namespace: NamespaceId,
}
impl CheapClone for Out {}

MkIndexer!(pub Id, u32);
#[derive(Debug, Clone, Copy)]
pub struct NamespaceId(Id);
#[derive(Debug, Clone, Copy)]
pub struct FunctionId(Id);
#[derive(Debug, Clone, Copy)]
pub struct TypedeclId(Id);
#[derive(Debug, Clone, Copy)]
pub struct TypecaseId(Id);
#[derive(Debug, Clone, Copy)]
pub struct LocalvecId(Id);
impl Ivec<Id, Item> {
    fn push_typecase(&mut self, c: Typecase) -> TypecaseId {
        TypecaseId(self.push(Item::Typecase(c)))
    }
    fn push_typedecl(&mut self, d: Typedecl) -> TypedeclId {
        TypedeclId(self.push(Item::Typedecl(d)))
    }
    fn push_function(&mut self, f: Function) -> FunctionId {
        FunctionId(self.push(Item::Function(f)))
    }
    fn push_namespace(&mut self, n: Namespace) -> NamespaceId {
        NamespaceId(self.push(Item::Namespace(n)))
    }
}
impl std::ops::Index<FunctionId> for Ivec<Id, Item> {
    type Output = Function;
    fn index(&self, FunctionId(id): FunctionId) -> &Self::Output {
        match &self[id] {
            Item::Function(f) => f,
            _ => panic!(),
        }
    }
}
impl std::ops::Index<TypedeclId> for Ivec<Id, Item> {
    type Output = Typedecl;

    fn index(&self, TypedeclId(id): TypedeclId) -> &Self::Output {
        match &self[id] {
            Item::Typedecl(d) => d,
            _ => panic!(),
        }
    }
}
impl std::ops::IndexMut<TypedeclId> for Ivec<Id, Item> {
    fn index_mut(&mut self, TypedeclId(id): TypedeclId) -> &mut Self::Output {
        match &mut self[id] {
            Item::Typedecl(d) => d,
            _ => panic!(),
        }
    }
}
impl std::ops::Index<NamespaceId> for Ivec<Id, Item> {
    type Output = Namespace;
    fn index(&self, NamespaceId(id): NamespaceId) -> &Self::Output {
        match &self[id] {
            Item::Namespace(n) => n,
            _ => panic!(),
        }
    }
}
impl std::ops::Index<LocalvecId> for Ivec<Id, Item> {
    type Output = Ivec<LocalId, Local>;
    fn index(&self, LocalvecId(id): LocalvecId) -> &Self::Output {
        match &self[id] {
            Item::Function(f) => &f.locals,
            Item::Typecase(c) => &c.locals,
            _ => panic!(),
        }
    }
}
impl std::ops::IndexMut<LocalvecId> for Ivec<Id, Item> {
    fn index_mut(&mut self, LocalvecId(id): LocalvecId) -> &mut Self::Output {
        match &mut self[id] {
            Item::Function(f) => &mut f.locals,
            Item::Typecase(c) => &mut c.locals,
            _ => panic!(),
        }
    }
}
impl std::ops::Index<TypecaseId> for Ivec<Id, Item> {
    type Output = Typecase;
    fn index(&self, TypecaseId(id): TypecaseId) -> &Self::Output {
        match &self[id] {
            Item::Typecase(c) => c,
            _ => panic!(),
        }
    }
}
impl From<TypedeclId> for Id {
    fn from(v: TypedeclId) -> Self {
        v.0
    }
}
impl From<TypecaseId> for Id {
    fn from(v: TypecaseId) -> Self {
        v.0
    }
}
impl From<FunctionId> for Id {
    fn from(v: FunctionId) -> Self {
        v.0
    }
}
impl Id {
    pub fn into_func(self) -> FunctionId {
        FunctionId(self)
    }
    pub fn into_typecase(self) -> TypecaseId {
        TypecaseId(self)
    }
    pub fn into_typedecl(self) -> TypedeclId {
        TypedeclId(self)
    }
    pub fn into_namespace(self) -> NamespaceId {
        NamespaceId(self)
    }
    pub fn into_localvec(self) -> LocalvecId {
        LocalvecId(self)
    }
}

MkIndexer!(pub LocalId, u32);
#[derive(Debug, Clone, Copy)]
pub struct BindingId(LocalId);
#[derive(Debug, Clone, Copy)]
pub struct ExprId(LocalId);
impl Ivec<LocalId, Local> {
    fn push_binding(&mut self, b: Binding) -> BindingId {
        BindingId(self.push(Local::Binding(b)))
    }
    fn push_expr(&mut self, e: Expr) -> ExprId {
        ExprId(self.push(Local::Expr(e)))
    }
}
impl std::ops::Index<(FunctionId, ExprId)> for Tara {
    type Output = Expr;
    fn index(&self, index: (FunctionId, ExprId)) -> &Self::Output {
        &self.quir_items[index.0].locals[index.1]
    }
}
impl std::ops::Index<BindingId> for Ivec<LocalId, Local> {
    type Output = Binding;
    fn index(&self, BindingId(id): BindingId) -> &Self::Output {
        match &self[id] {
            Local::Binding(b) => b,
            _ => panic!(),
        }
    }
}
impl std::ops::IndexMut<BindingId> for Ivec<LocalId, Local> {
    fn index_mut(&mut self, BindingId(id): BindingId) -> &mut Self::Output {
        match &mut self[id] {
            Local::Binding(b) => b,
            _ => panic!(),
        }
    }
}
impl std::ops::Index<ExprId> for Ivec<LocalId, Local> {
    type Output = Expr;
    fn index(&self, ExprId(id): ExprId) -> &Self::Output {
        match &self[id] {
            Local::Expr(e) => e,
            _ => panic!(),
        }
    }
}
impl std::ops::IndexMut<ExprId> for Ivec<LocalId, Local> {
    fn index_mut(&mut self, ExprId(id): ExprId) -> &mut Self::Output {
        match &mut self[id] {
            Local::Expr(e) => e,
            _ => panic!(),
        }
    }
}

MkIndexer!(pub TypeId, u32);
impl Tara {
    pub fn intern(&mut self, kind: Typekind) -> TypeId {
        match self.quir_interns.get(&kind) {
            Some(id) => *id,
            None => {
                let id = self.quir_types.push(kind.cheap());
                self.quir_interns.insert(kind, id);
                id
            }
        }
    }
    fn new_tvar(&mut self) -> TypeId {
        // doesn't need interning as it's unique anyway
        let var = self.quir_tvars;
        self.quir_tvars += 1;
        self.quir_types.push(Typekind::Var(var))
    }
    pub fn tuple(&mut self, fields: Rc<[TypeId]>) -> TypeId {
        let bundle = self.intern(Typekind::Bundle(fields.cheap()));
        let tup = self.intern(Typekind::Tup);
        self.intern(Typekind::Call {
            args: bundle,
            func: tup,
        })
    }
    thread_local! {
        static NO_FIELDS: Rc<[TypeId]> = Rc::new([]);
    }
    pub fn unit(&mut self) -> TypeId {
        self.tuple(Self::NO_FIELDS.with(Rc::cheap))
    }
}

#[derive(Debug)]
pub enum Item {
    Typecase(Typecase),
    Typedecl(Typedecl),
    Function(Function),
    Namespace(Namespace),
}
impl Item {
    pub fn loc(&self) -> Provenance {
        match self {
            // pass back name loc, to avoid showing entire item
            Item::Typecase(t) => t.name.loc,
            Item::Typedecl(t) => t.name.loc,
            Item::Function(f) => f.name.loc,
            Item::Namespace(_) => panic!("namespaces cannot yet have any locations"),
        }
    }

    pub fn module(&self) -> ModuleId {
        match self {
            Item::Typecase(t) => t.module,
            Item::Typedecl(t) => t.module,
            Item::Function(f) => f.module,
            Item::Namespace(_) => todo!("I have no clue what to do with namespaces and modules"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub items: Rc<Map<Istr, Id>>,
}
impl CheapClone for Namespace {}

#[derive(Debug, Clone)]
pub struct Typedecl {
    pub loc: Provenance,
    pub name: Ident,
    pub cases: Rc<[TypecaseId]>,
    pub module: ModuleId,
}
impl CheapClone for Typedecl {}

#[derive(Debug)]
pub struct Typecase {
    pub loc: Provenance,
    pub name: Ident,
    pub typ: Type,
    pub binding: BindingId,
    pub parent: TypedeclId,
    pub index: usize,
    pub locals: Ivec<LocalId, Local>,
    pub module: ModuleId,
}

#[derive(Debug)]
pub struct Function {
    pub loc: Provenance,
    pub name: Ident,
    pub typ: Type,
    pub body: ExprId,
    pub locals: Ivec<LocalId, Local>,
    pub module: ModuleId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Type {
    pub loc: Provenance,
    pub kind: TypeId,
}
impl Type {
    pub fn fmt<'a>(&self, tara: &'a Tara) -> TypeFmt<'a> {
        TypeFmt {
            this: self.kind,
            types: &tara.quir_types,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Typekind {
    Func { args: TypeId, ret: TypeId },
    Call { args: TypeId, func: TypeId },
    Bundle(Rc<[TypeId]>),
    // we tag recall with ModuleId,
    // as interning types may deduplicate
    // a recall across modules, causing later
    // name resolution to modify recalls with
    // the same name, but differenc contexts
    Recall(Istr, ModuleId, Option<Id>),
    Var(usize),
    String,
    Bool,
    Int,
    Tup,
}
impl CheapClone for Typekind {}

pub struct TypeFmt<'a> {
    this: TypeId,
    types: &'a Ivec<TypeId, Typekind>,
}
impl TypeFmt<'_> {
    fn sub(&self, id: TypeId) -> Self {
        Self {
            this: id,
            types: self.types,
        }
    }
}
impl std::fmt::Display for TypeFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.types[self.this] {
            Typekind::Func { args, ret } => {
                write!(f, "func({}): {}", self.sub(args), self.sub(ret))
            }
            Typekind::Call { args, func } => write!(f, "{}{}", self.sub(func), self.sub(args)),
            Typekind::Bundle(ref ids) => {
                write!(f, "(")?;
                if !ids.is_empty() {
                    write!(f, "{}", self.sub(ids[0]))?;
                    for &i in &ids[1..] {
                        write!(f, ", {}", self.sub(i))?;
                    }
                }
                write!(f, ")")
            }
            Typekind::Recall(istr, _, _) => write!(f, "{}", istr.0),
            Typekind::Var(n) => write!(f, "?{}", n),
            Typekind::String => write!(f, "__string"),
            Typekind::Bool => write!(f, "__bool"),
            Typekind::Int => write!(f, "__int"),
            Typekind::Tup => write!(f, "__tuple"),
        }
    }
}

CommonEnum! {
    #[derive(Debug)]
    pub enum Local {
        Expr: Expr,
        Binding: Binding,
    }
}

macro_rules! MkSimpleEnum {
    ($vis:vis $enum:ident : $($name:ident),* $(,)?) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
        $vis enum $enum {
            $($name),*
        }
        impl $enum {
            pub const VALUES: &'static [$enum] = {
                use $enum::*;
                &[
                    $($name),*
                ]
            };
        }
    };
}

#[derive(Debug)]
pub struct Binding {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: binding::Kind,
}

pub mod binding {
    use crate::{codegen, fill};

    use super::*;

    CommonEnum! {
        #[derive(Debug, Clone)]
        pub enum Kind {
            Empty: Empty,
            Name: Name,
            Tuple: Tuple,
        }
        pub(in crate::tara) &self.fill(ctx: &mut fill::Ctx) -> ();
        pub(in crate::tara) &self.codegen(
            v: inkwell::values::BasicValueEnum<'static>,
            ctx: &mut codegen::Ctx
        ) -> ();
    }
    impl CheapClone for Kind {}

    #[derive(Debug, Clone, Copy)]
    pub struct Empty;
    #[derive(Debug, Clone, Copy)]
    pub struct Name(pub Istr, pub bool);
    #[derive(Debug, Clone)]
    pub struct Tuple(pub Rc<[BindingId]>);
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Provenance,
    pub typ: Type,
    pub kind: expr::Kind,
}
impl CheapClone for Expr {}

pub mod expr {
    use crate::{codegen, fill, resolve, typer};

    use super::*;
    use either::Either;

    CommonEnum! {
        #[derive(Debug, Clone)]
        pub enum Kind {
            If: If,
            Call: Call,
            Builtin: Builtinkind,
            Tuple: Tuple,
            Loop: Loop,
            Bareblock: Bareblock,
            Recall: Recall,
            Number: Number,
            String: String,
            Bool: Bool,
            Arguments: Arguments,
            Poison: Poison,
            Let: Let,
            Assign: Assign,
            Break: Break,
            Return: Return,
            Const: Const,
        }
        pub(in crate::tara) &self.typecheck(
            loc: Provenance,
            typ: Type,
            ctx: &mut typer::Ctx
        ) -> Result<Type, typer::ControlFlow>;
        pub(in crate::tara) &self.resolve(id: ExprId, ctx: &mut resolve::Ctx) -> ();
        pub(in crate::tara) &self.fill(ctx: &mut fill::Ctx) -> ();
        pub(in crate::tara) &self.codegen(typ: Type, ctx: &mut codegen::Ctx) -> codegen::Result;
    }
    impl CheapClone for Kind {}

    #[derive(Debug, Clone)]
    pub struct If {
        pub cond: ExprId,
        pub smash: ExprId,
        pub pass: ExprId,
    }

    #[derive(Debug, Clone)]
    pub struct Call {
        pub func: ExprId,
        pub args: ExprId,
    }

    MkSimpleEnum! {pub Builtinkind:
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        And,
        Or,
        Xor,
        ShLeft,
        ShRight,
        Not,
        Negate,
        CmpEq,
        CmpNE,
        CmpGt,
        CmpLt,
        CmpGE,
        CmpLE,
        Syscall,
        PtrToInt,
        IntToPtr
    }
    impl Builtinkind {
        pub const fn spelling(self) -> &'static str {
            match self {
                Builtinkind::Add => "__builtin_add",
                Builtinkind::Sub => "__builtin_sub",
                Builtinkind::Mul => "__builtin_mul",
                Builtinkind::Div => "__builtin_div",
                Builtinkind::Mod => "__builtin_mod",
                Builtinkind::And => "__builtin_and",
                Builtinkind::Or => "__builtin_or",
                Builtinkind::Xor => "__builtin_xor",
                Builtinkind::ShLeft => "__builtin_shl",
                Builtinkind::ShRight => "__builtin_shr",
                Builtinkind::Not => "__builtin_not",
                Builtinkind::Negate => "__builtin_negate",
                Builtinkind::CmpEq => "__builtin_cmp_eq",
                Builtinkind::CmpNE => "__builtin_cmp_ne",
                Builtinkind::CmpGt => "__builtin_cmp_gt",
                Builtinkind::CmpLt => "__builtin_cmp_lt",
                Builtinkind::CmpGE => "__builtin_cmp_ge",
                Builtinkind::CmpLE => "__builtin_cmp_le",
                Builtinkind::Syscall => "__builtin_syscall",
                Builtinkind::PtrToInt => "__builtin_ptr_to_int",
                Builtinkind::IntToPtr => "__builtin_int_to_ptr",
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Tuple(pub Rc<[ExprId]>);
    #[derive(Debug, Clone)]
    pub struct Loop(pub ExprId);
    #[derive(Debug, Clone)]
    pub struct Bareblock(pub Rc<[ExprId]>);
    #[derive(Debug, Clone)]
    pub struct Recall(pub Rc<[Ident]>, pub Option<Either<BindingId, Id>>);
    #[derive(Debug, Clone)]
    pub struct Number(pub Istr);
    #[derive(Debug, Clone)]
    pub struct String(pub Istr);
    #[derive(Debug, Clone)]
    pub struct Bool(pub Istr);
    #[derive(Debug, Clone)]
    pub struct Arguments;
    #[derive(Debug, Clone)]
    pub struct Poison;

    #[derive(Debug, Clone)]
    pub struct Let(pub BindingId, pub ExprId);
    #[derive(Debug, Clone)]
    pub struct Assign(pub Ident, pub Option<BindingId>, pub ExprId);
    #[derive(Debug, Clone)]
    pub struct Break {
        pub val: ExprId,
        pub target: Option<ExprId>,
    }
    #[derive(Debug, Clone)]
    pub struct Return(pub ExprId);
    #[derive(Debug, Clone)]
    pub struct Const(pub ExprId);
}

gen_query!(quir);
fn quir(tara: &mut Tara, i: In) -> Out {
    let ast = tara.parse(parse::In { m: i.m });
    let mut items = Map::new();
    for t in ast.ast.types.iter() {
        let name = t.name.name;
        let t = t.convert(i.m, tara);
        items.insert(name, t.into());
    }
    for f in ast.ast.funcs.iter() {
        let name = f.name.name;
        let f = f.convert(i.m, tara);
        items.insert(name, f.into());
    }
    let namespace = tara.quir_items.push_namespace(Namespace {
        items: items.into(),
    });
    Out { namespace }
}

impl parse::Typedecl {
    thread_local! {
        static NO_CASES: Rc<[TypecaseId]> = Rc::new([]);
    }
    fn convert(&self, m: ModuleId, tara: &mut Tara) -> TypedeclId {
        let id = tara.quir_items.push_typedecl(Typedecl {
            loc: self.loc,
            name: self.name,
            cases: Self::NO_CASES.with(Rc::clone),
            module: m,
        });
        let mut cases = vec![];
        for (i, c) in self.cases.iter().enumerate() {
            let c = c.convert(m, id, i, tara);
            cases.push(c);
        }
        tara.quir_items[id].cases = cases.into();
        id
    }
}

impl parse::Typecase {
    fn convert(
        &self,
        m: ModuleId,
        parent: TypedeclId,
        index: usize,
        tara: &mut Tara,
    ) -> TypecaseId {
        let mut locals = Ivec::default();
        let binding = self.binding.convert(
            false,
            &mut GCtx {
                locals: &mut locals,
                module: m,
                tara,
            },
        );
        let typ = locals[binding].typ;
        let ret = tara.intern(Typekind::Recall(
            tara.quir_items[parent].name.name,
            m,
            Some(parent.into())
        ));
        let typ = Type {
            kind: tara.intern(Typekind::Func {
                args: typ.kind,
                ret,
            }),
            loc: typ.loc,
        };
        let c = Typecase {
            loc: self.loc,
            name: self.name,
            module: m,
            binding,
            parent,
            locals,
            index,
            typ,
        };
        tara.quir_items.push_typecase(c)
    }
}

impl parse::Function {
    fn convert(&self, m: ModuleId, tara: &mut Tara) -> FunctionId {
        let mut locals = Ivec::default();
        let bind = self.args.convert(
            false,
            &mut GCtx {
                locals: &mut locals,
                module: m,
                tara,
            },
        );
        let block = self.body.convert(&mut ECtx {
            locals: &mut locals,
            module: m,
            tara,
        });
        let args_t = locals[bind].typ;
        let args = locals.push_expr(Expr {
            kind: expr::Arguments.into(),
            loc: self.args.loc,
            typ: args_t,
        });
        let bind = locals.push_expr(Expr {
            typ: Type {
                loc: self.args.loc,
                kind: tara.unit(),
            },
            kind: expr::Let(bind, args).into(),
            loc: self.args.loc,
        });
        let body = locals.push_expr(Expr {
            typ: locals[block].typ,
            kind: expr::Bareblock(Rc::new([bind, block])).into(),
            loc: self.args.loc,
        });
        let ret_t = self.ret.convert(m, tara);
        let typ = Type {
            kind: tara.intern(Typekind::Func {
                args: args_t.kind,
                ret: ret_t.kind,
            }),
            loc: self.args.loc.meet(&ret_t.loc),
        };
        tara.quir_items.push_function(Function {
            name: self.name,
            loc: self.loc,
            module: m,
            locals,
            body,
            typ,
        })
    }
}

pub(super) trait Ctx {
    fn module(&self) -> ModuleId;
    fn locals(&mut self) -> &mut Ivec<LocalId, Local>;
    fn some_type(&mut self) -> TypeId;
    fn tara(&mut self) -> &mut Tara;
}

pub(super) struct ECtx<'a> {
    module: ModuleId,
    locals: &'a mut Ivec<LocalId, Local>,
    tara: &'a mut Tara,
}
impl Ctx for ECtx<'_> {
    fn module(&self) -> ModuleId {
        self.module
    }
    fn locals(&mut self) -> &mut Ivec<LocalId, Local> {
        self.locals
    }
    fn some_type(&mut self) -> TypeId {
        self.tara.new_tvar()
    }
    fn tara(&mut self) -> &mut Tara {
        self.tara
    }
}

struct GCtx<'a> {
    module: ModuleId,
    locals: &'a mut Ivec<LocalId, Local>,
    tara: &'a mut Tara,
}
impl Ctx for GCtx<'_> {
    fn module(&self) -> ModuleId {
        self.module
    }
    fn tara(&mut self) -> &mut Tara {
        self.tara
    }
    fn locals(&mut self) -> &mut Ivec<LocalId, Local> {
        self.locals
    }
    // no type annotations in the global context
    // resolve to the unit type, as we only want to
    // perform type inference locally
    fn some_type(&mut self) -> TypeId {
        self.tara.unit()
    }
}

impl parse::Binding {
    fn convert(&self, mutable: bool, ctx: &mut dyn Ctx) -> BindingId {
        self.kind.convert(mutable, self.loc, ctx)
    }
}

impl parse::binding::Empty {
    pub(super) fn convert(&self, _: bool, loc: Provenance, ctx: &mut dyn Ctx) -> BindingId {
        let typ = Type {
            kind: ctx.some_type(),
            loc,
        };
        ctx.locals().push_binding(Binding {
            kind: binding::Empty.into(),
            typ,
            loc,
        })
    }
}

impl parse::binding::Name {
    pub(super) fn convert(&self, mutable: bool, loc: Provenance, ctx: &mut dyn Ctx) -> BindingId {
        let typ = match self.1 {
            Some(ref t) => t.convert(ctx.module(), ctx.tara()),
            None => Type {
                kind: ctx.some_type(),
                loc,
            },
        };
        ctx.locals().push_binding(Binding {
            kind: binding::Name(self.0, mutable).into(),
            typ,
            loc,
        })
    }
}

impl parse::binding::Tuple {
    pub(super) fn convert(&self, mutable: bool, loc: Provenance, ctx: &mut dyn Ctx) -> BindingId {
        let (ts, bs): (Vec<_>, Vec<_>) = self
            .0
            .iter()
            .map(|b| {
                let b = b.convert(mutable, ctx);
                (ctx.locals()[b].typ.kind, b)
            })
            .unzip();
        let typ = Type {
            kind: ctx.tara().tuple(ts.into()),
            loc,
        };
        ctx.locals().push_binding(Binding {
            kind: binding::Tuple(bs.into()).into(),
            typ,
            loc,
        })
    }
}

impl parse::Type {
    fn convert(&self, m: ModuleId, tara: &mut Tara) -> Type {
        let loc = self.loc;
        match self.kind {
            parse::Typekind::Func { ref args, ref ret } => {
                let args = args.convert(m, tara).kind;
                let ret = ret.convert(m, tara).kind;
                Type {
                    kind: tara.intern(Typekind::Func { args, ret }),
                    loc,
                }
            }
            parse::Typekind::Call { ref args, ref func } => {
                let func = func.convert(m, tara).kind;
                let args = args.convert(m, tara).kind;
                Type {
                    kind: tara.intern(Typekind::Call { func, args }),
                    loc,
                }
            }
            parse::Typekind::Bundle(ref fields) => {
                let fields: Vec<_> = fields.iter().map(|t| t.convert(m, tara).kind).collect();
                Type {
                    kind: tara.intern(Typekind::Bundle(fields.into())),
                    loc,
                }
            }
            parse::Typekind::Recall(name) => Type {
                kind: tara.intern(Typekind::Recall(name, m, None)),
                loc,
            },
        }
    }
}

impl parse::Expr {
    fn convert(&self, ctx: &mut ECtx) -> ExprId {
        self.kind.convert(self.loc, ctx)
    }
}

impl parse::expr::If {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let cond = self.cond.convert(ctx);
        let smash = self.smash.convert(ctx);
        let pass = self.pass.convert(ctx);
        let typ = Type {
            kind: ctx.some_type(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::If { cond, smash, pass }.into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Call {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let func = self.func.convert(ctx);
        let args = self.args.convert(ctx);
        let typ = Type {
            kind: ctx.some_type(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Call { func, args }.into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Tuple {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let (types, exprs): (Vec<_>, Vec<_>) = self
            .0
            .iter()
            .map(|e| {
                let e = e.convert(ctx);
                (ctx.locals()[e].typ.kind, e)
            })
            .unzip();
        let typ = Type {
            kind: ctx.tara().tuple(types.into()),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Tuple(exprs.into()).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Loop {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let body = self.0.convert(ctx);
        let typ = Type {
            kind: ctx.some_type(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Loop(body).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Bareblock {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let exprs: Vec<_> = self.0.iter().map(|e| e.convert(ctx)).collect();
        let typ = Type {
            kind: ctx.some_type(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Bareblock(exprs.into()).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Path {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let typ = Type {
            kind: ctx.some_type(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Recall(self.0.cheap(), None).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Number {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let typ = Type {
            kind: ctx.tara().intern(Typekind::Int),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Number(self.0).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::String {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let typ = Type {
            kind: ctx.tara().intern(Typekind::String),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::String(self.0).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Bool {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let typ = Type {
            kind: ctx.tara().intern(Typekind::Bool),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Bool(self.0).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Let {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let binding = self.0.convert(false, ctx);
        let expr = self.1.convert(ctx);
        let typ = Type {
            kind: ctx.tara().unit(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Let(binding, expr).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Mut {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let binding = self.0.convert(true, ctx);
        let expr = self.1.convert(ctx);
        let typ = Type {
            kind: ctx.tara().unit(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Let(binding, expr).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Assign {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let expr = self.1.convert(ctx);
        let typ = Type {
            kind: ctx.tara().unit(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Assign(self.0, None, expr).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Break {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let typ = Type {
            kind: ctx.tara().unit(),
            loc,
        };
        let val = match self.0 {
            Some(ref val) => val.convert(ctx),
            None => ctx.locals().push_expr(Expr {
                kind: expr::Tuple(Rc::new([])).into(),
                typ,
                loc,
            }),
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Break { val, target: None }.into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Return {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let typ = Type {
            kind: ctx.tara().unit(),
            loc,
        };
        let val = match self.0 {
            Some(ref val) => val.convert(ctx),
            None => ctx.locals().push_expr(Expr {
                kind: expr::Tuple(Rc::new([])).into(),
                typ,
                loc,
            }),
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Return(val).into(),
            typ,
            loc,
        })
    }
}

impl parse::expr::Const {
    pub(super) fn convert(&self, loc: Provenance, ctx: &mut ECtx) -> ExprId {
        let val = self.0.convert(ctx);
        let typ = Type {
            kind: ctx.tara().unit(),
            loc,
        };
        ctx.locals().push_expr(Expr {
            kind: expr::Const(val).into(),
            typ,
            loc,
        })
    }
}
