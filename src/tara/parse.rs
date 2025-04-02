use std::fmt::Write;
use std::rc::Rc;

use crate::{
    gen_query, message,
    misc::{CheapClone, Istr},
    preimport, prescan,
    tokens::{Token, Tokenkind},
    FmtMessage, Provenance,
};

use super::{prescan::Opdef, ModuleId, Tara};

#[derive(Clone, Debug)]
pub struct Out {
    pub ast: Rc<Ast>,
}
impl CheapClone for Out {}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct In {
    pub m: ModuleId,
}

#[derive(Debug)]
pub struct Ast {
    pub funcs: Vec<Function>,
    pub types: Vec<Typedecl>,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub name: Istr,
    pub loc: Provenance,
}

#[derive(Debug)]
pub struct Function {
    pub loc: Provenance,
    pub name: Ident,
    pub ret: Type,
    pub args: Binding,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Typedecl {
    pub loc: Provenance,
    pub name: Ident,
    pub cases: Vec<Typecase>,
}

#[derive(Debug)]
pub struct Typecase {
    pub loc: Provenance,
    pub name: Ident,
    pub binding: Binding,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub loc: Provenance,
    pub kind: Typekind,
}
impl Type {
    pub fn unit(loc: Provenance) -> Self {
        Type {
            kind: Typekind::Call {
                args: Box::new(Type {
                    kind: Typekind::Bundle(Vec::new()),
                    loc,
                }),
                func: Box::new(Type {
                    kind: Typekind::Recall("tuple".into()),
                    loc,
                }),
            },
            loc,
        }
    }
}
#[derive(Clone, Debug)]
pub enum Typekind {
    Func { args: Box<Type>, ret: Box<Type> },
    Call { args: Box<Type>, func: Box<Type> },
    Bundle(Vec<Type>),
    Recall(Istr),
}
impl Default for Typekind {
    fn default() -> Self {
        Typekind::Bundle(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub loc: Provenance,
    // pub kind: Box<dyn uir::BindingT>,
    pub kind: binding::Kind,
}

pub mod binding {
    use super::*;
    use crate::{quir, CommonEnum};

    CommonEnum! {
        #[derive(Debug, Clone)]
        pub enum Kind {
            Empty: Empty,
            Name: Name,
            Tuple: Tuple,
        }
        pub(in crate::tara) &self.convert(mutable: bool, loc: Provenance, ctx: &mut dyn quir::Ctx) -> quir::BindingId;
    }

    #[derive(Debug, Clone)]
    pub struct Empty;
    #[derive(Debug, Clone)]
    pub struct Name(pub Istr, pub Option<Type>);
    #[derive(Debug, Clone)]
    pub struct Tuple(pub Vec<Binding>);
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub loc: Provenance,
    // pub kind: Rc<dyn uir::ExprT>,
    pub kind: expr::Exprkind,
}

pub mod expr {
    use super::*;
    use crate::{quir, CommonEnum};

    CommonEnum! {
        #[derive(Debug, Clone)]
        pub enum Exprkind {
            If: Box<If>,
            Call: Box<Call>,
            Tuple: Box<Tuple>,
            Loop: Box<Loop>,
            Bareblock: Box<Bareblock>,
            Path: Box<Path>,
            Number: Box<Number>,
            String: Box<String>,
            Bool: Box<Bool>,
            Let: Box<Let>,
            Mut: Box<Mut>,
            Assign: Box<Assign>,
            Break: Box<Break>,
            Return: Box<Return>,
            Const: Box<Const>,
        }

        pub(in crate::tara) &self.convert(loc: Provenance, ctx: &mut quir::ECtx) -> quir::ExprId;
    }

    #[derive(Debug, Clone)]
    pub struct If {
        pub cond: Expr,
        pub smash: Expr,
        pub pass: Expr,
    }

    #[derive(Debug, Clone)]
    pub struct Call {
        pub func: Expr,
        pub args: Expr,
    }

    #[derive(Debug, Clone)]
    pub struct Tuple(pub Vec<Expr>);
    #[derive(Debug, Clone)]
    pub struct Loop(pub Expr);
    #[derive(Debug, Clone)]
    pub struct Bareblock(pub Vec<Expr>);

    #[derive(Debug, Clone)]
    pub struct Path(pub Rc<[Ident]>);
    #[derive(Debug, Clone)]
    pub struct Number(pub Istr);
    #[derive(Debug, Clone)]
    pub struct String(pub Istr);
    #[derive(Debug, Clone)]
    pub struct Bool(pub Istr);

    #[derive(Debug, Clone)]
    pub struct Let(pub Binding, pub Expr);
    #[derive(Debug, Clone)]
    pub struct Mut(pub Binding, pub Expr);
    #[derive(Debug, Clone)]
    pub struct Assign(pub Ident, pub Expr);
    #[derive(Debug, Clone)]
    pub struct Break(pub Option<Expr>);
    #[derive(Debug, Clone)]
    pub struct Return(pub Option<Expr>);
    #[derive(Debug, Clone)]
    pub struct Const(pub Expr);
}

gen_query!(parse);
fn parse(ctx: &mut Tara, i: In) -> Out {
    let pp = ctx.prescan(prescan::In { m: i.m });
    let pi = ctx.preimport(preimport::In { m: i.m });
    let mut parser = Parser {
        ctx,
        m: i.m,
        tokens: pp.tokens.as_ref(),
        ops: pi.ops,
    };
    let ast = parser.top();
    Out { ast: Rc::new(ast) }
}

struct Parser<'a> {
    ctx: &'a mut Tara,
    m: ModuleId,
    tokens: &'a [Token<Istr>],
    ops: Rc<[Opdef]>,
}

impl Parser<'_> {
    fn n_peek(&self, n: usize) -> Option<Token<Istr>> {
        self.tokens.get(n).copied()
    }
    fn peek(&self) -> Option<Token<Istr>> {
        self.tokens.first().copied()
    }

    fn n_is(&self, n: usize, k: Tokenkind) -> bool {
        self.n_peek(n).is_some_and(|t| t.kind == k)
    }
    fn next_is(&self, k: Tokenkind) -> bool {
        self.peek().is_some_and(|t| t.kind == k)
    }
    fn next_is_text(&self, s: Istr) -> bool {
        self.peek().is_some_and(|t| t.text == s)
    }

    fn dispatch<T>(
        &mut self,
        if_: &[Tokenkind],
        then: &mut [&mut dyn FnMut(&mut Self) -> T],
        else_: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let Some(t) = self.peek() else {
            return else_(self);
        };
        for (i, &k) in if_.iter().enumerate() {
            if k == t.kind {
                return then[i](self);
            }
        }
        else_(self)
    }

    fn check(&mut self, k: Tokenkind) -> Option<Token<Istr>> {
        if !self.next_is(k) {
            return None;
        }
        Some(self.eat())
    }
    fn check_text(&mut self, t: Istr) -> Option<Token<Istr>> {
        if !self.next_is_text(t) {
            return None;
        }
        Some(self.eat())
    }

    fn eat(&mut self) -> Token<Istr> {
        if self.tokens.is_empty() {
            self.unexpected_token(None, &[], &[]);
        }
        let out = self.tokens[0];
        self.tokens = &self.tokens[1..];
        out
    }
    fn expect(&mut self, k: Tokenkind, notes: &[FmtMessage]) -> Token<Istr> {
        if !self.next_is(k) {
            self.unexpected_token(self.peek(), &[k], notes);
            std::process::exit(1);
        }
        self.eat()
    }

    fn top(&mut self) -> Ast {
        let mut ast = Ast {
            funcs: Vec::new(),
            types: Vec::new(),
        };
        while !self.tokens.is_empty() {
            self.decls(&mut ast);
        }
        ast
    }
    fn decls(&mut self, ast: &mut Ast) {
        let cases = &[Tokenkind::Func, Tokenkind::Type];
        match self.dispatch(
            cases,
            &mut [&mut |s| Ok(s.function()), &mut |s| Err(s.decls_type())],
            |s| {
                s.unexpected_token(s.peek(), cases, &[]);
                std::process::exit(1)
            },
        ) {
            Ok(f) => ast.funcs.push(f),
            Err(t) => ast.types.push(t),
        }
    }

    fn decls_type(&mut self) -> Typedecl {
        let mut loc = self.expect(Tokenkind::Type, &[]).loc;
        let name = self.expect(Tokenkind::Name, &[]);
        let cases = if self.check(Tokenkind::Equals).is_some() {
            let def = self.type_variant();
            loc = loc.meet(&def.loc);
            vec![def]
        } else {
            let (def, l) = self.type_body();
            loc = loc.meet(&l);
            def
        };
        Typedecl {
            name: Ident {
                name: name.text,
                loc: name.loc,
            },
            cases,
            loc,
        }
    }

    fn type_body(&mut self) -> (Vec<Typecase>, Provenance) {
        let loc = self.expect(Tokenkind::OpenBrace, &[]).loc;
        let mut cases = Vec::new();
        while !self.next_is(Tokenkind::CloseBrace) {
            let var = self.type_variant();
            cases.push(var);
        }
        let loc = loc.meet(&self.expect(Tokenkind::CloseBrace, &[]).loc);
        self.check(Tokenkind::Semicolon);
        (cases, loc)
    }

    fn type_variant(&mut self) -> Typecase {
        let loc = self.expect(Tokenkind::Case, &[]).loc;
        let name = self.expect(Tokenkind::Name, &[]);
        let binding = self.binding_parenthesised();
        let loc = loc.meet(&self.expect(Tokenkind::Semicolon, &[]).loc);
        Typecase {
            loc,
            name: Ident {
                name: name.text,
                loc: name.loc,
            },
            binding,
        }
    }

    fn function(&mut self) -> Function {
        let loc = self.expect(Tokenkind::Func, &[]).loc;
        let name = self.expect(Tokenkind::Name, &[]);
        let args = self.binding_parenthesised();
        let ret = if self.check(Tokenkind::Colon).is_some() {
            self.type_()
        } else {
            Type::unit(args.loc)
        };
        let body = if self.check(Tokenkind::Equals).is_some() {
            self.expr_delimited()
        } else {
            let body = self.expr_bareblock();
            self.check(Tokenkind::Semicolon);
            body
        };
        let loc = loc.meet(&body.loc);
        Function {
            loc,
            name: Ident {
                name: name.text,
                loc: name.loc,
            },
            ret,
            args,
            body,
        }
    }

    fn binding(&mut self) -> Binding {
        self.binding_atom()
    }

    fn binding_atom(&mut self) -> Binding {
        if self.next_is(Tokenkind::OpenParen) {
            self.binding_parenthesised()
        } else {
            self.binding_name()
        }
    }

    fn binding_name(&mut self) -> Binding {
        let name = self.expect(Tokenkind::Name, &[]);
        let mut loc = name.loc;
        let typ = if self.check(Tokenkind::Colon).is_some() {
            let t = self.type_();
            loc = loc.meet(&t.loc);
            Some(t)
        } else {
            None
        };
        Binding {
            loc,
            kind: binding::Name(name.text, typ).into(),
        }
    }

    fn binding_parenthesised(&mut self) -> Binding {
        let loc = self.expect(Tokenkind::OpenParen, &[]).loc;
        let mut fields = Vec::new();
        if !self.next_is(Tokenkind::CloseParen) {
            fields.push(self.binding());
            while self.check(Tokenkind::Comma).is_some() {
                fields.push(self.binding());
            }
        }
        let loc = loc.meet(&self.expect(Tokenkind::CloseParen, &[]).loc);
        match fields.len() {
            0 => Binding {
                loc,
                kind: binding::Empty.into(),
            },
            1 => fields.pop().unwrap(),
            _ => Binding {
                loc,
                kind: binding::Tuple(fields).into(),
            },
        }
    }

    fn type_(&mut self) -> Type {
        if self.next_is(Tokenkind::Func) {
            return self.type_func();
        }
        self.type_op(0)
    }

    fn type_func(&mut self) -> Type {
        let loc = self.expect(Tokenkind::Func, &[]).loc;
        let args = Box::new(self.type_parenthesised());
        let ret = if self.check(Tokenkind::Colon).is_some() {
            self.type_()
        } else {
            Type::unit(args.loc)
        };
        let ret = Box::new(ret);
        let loc = loc.meet(&ret.loc);
        Type {
            loc,
            kind: Typekind::Func { args, ret },
        }
    }

    fn type_op(&mut self, prec: u32) -> Type {
        let mut left = if self.type_op_left_first() {
            self.type_op_left()
        } else {
            self.type_atom()
        };
        while self.type_op_right_first(prec) {
            left = self.type_op_right(prec, left);
        }
        left
    }

    fn type_op_left_first(&mut self) -> bool {
        for o in self.ops.clone().iter() {
            if o.lbp.is_some() {
                continue;
            }
            if self.next_is_text(o.spelling) {
                return true;
            }
        }
        false
    }

    fn type_op_left(&mut self) -> Type {
        for o in self.ops.clone().iter() {
            if o.lbp.is_some() {
                continue;
            }
            if let Some(t) = self.check_text(o.spelling) {
                let args = Box::new(self.type_op(o.rbp.unwrap().get()));
                return Type {
                    loc: t.loc.meet(&args.loc),
                    kind: Typekind::Call {
                        args,
                        func: Box::new(Type {
                            loc: t.loc,
                            kind: Typekind::Recall(t.text),
                        }),
                    },
                };
            }
        }
        self.unexpected_token(self.peek(), &[], &[]);
        std::process::exit(1);
    }

    fn type_op_right_first(&self, prec: u32) -> bool {
        if self.next_is(Tokenkind::OpenParen) {
            return true;
        }
        for o in self.ops.clone().iter() {
            if o.lbp.map_or(true, |lbp| lbp.get() < prec) {
                continue;
            }
            if self.next_is_text(o.spelling) {
                return true;
            }
        }
        false
    }

    fn type_op_right(&mut self, prec: u32, left: Type) -> Type {
        if self.next_is(Tokenkind::OpenParen) {
            let args = Box::new(self.type_parenthesised());
            return Type {
                loc: left.loc.meet(&args.loc),
                kind: Typekind::Call {
                    args,
                    func: Box::new(left),
                },
            };
        }
        for o in self.ops.clone().iter() {
            if o.lbp.map_or(true, |lbp| lbp.get() < prec) {
                continue;
            }
            if let Some(t) = self.check_text(o.spelling) {
                let args = if let Some(rbp) = o.rbp {
                    let right = self.type_op(rbp.get());
                    Box::new(Type {
                        loc: left.loc.meet(&right.loc),
                        kind: Typekind::Bundle(vec![left, right]),
                    })
                } else {
                    Box::new(left)
                };
                return Type {
                    loc: args.loc.meet(&t.loc),
                    kind: Typekind::Call {
                        args,
                        func: Box::new(Type {
                            loc: t.loc,
                            kind: Typekind::Recall(t.text),
                        }),
                    },
                };
            }
        }
        unreachable!("because loops on type_op_right_first()");
    }

    fn type_atom(&mut self) -> Type {
        if self.next_is(Tokenkind::OpenParen) {
            self.type_parenthesised()
        } else {
            let t = self.expect(Tokenkind::Name, &[]);
            Type {
                loc: t.loc,
                kind: Typekind::Recall(t.text),
            }
        }
    }

    fn type_parenthesised(&mut self) -> Type {
        let loc = self.expect(Tokenkind::OpenParen, &[]).loc;
        let mut fields = Vec::new();
        if !self.next_is(Tokenkind::CloseParen) {
            fields.push(self.type_());
            if self.check(Tokenkind::Semicolon).is_some() {
                let t = fields.pop().unwrap();
                let n = self.expect(Tokenkind::Number, &[]);
                for _ in 0..n.text.parse::<usize>().unwrap() {
                    fields.push(t.clone());
                }
            }
            while self.check(Tokenkind::Comma).is_some() {
                fields.push(self.type_());
                if self.check(Tokenkind::Semicolon).is_some() {
                    let t = fields.pop().unwrap();
                    let n = self.expect(Tokenkind::Number, &[]);
                    for _ in 0..n.text.parse::<usize>().unwrap() {
                        fields.push(t.clone());
                    }
                }
            }
        }
        let loc = loc.meet(&self.expect(Tokenkind::CloseParen, &[]).loc);
        match fields.len() {
            1 => fields.pop().unwrap(),
            _ => Type {
                loc,
                kind: Typekind::Bundle(fields),
            },
        }
    }

    fn expr_delimited(&mut self) -> Expr {
        self.dispatch(
            &[Tokenkind::If, Tokenkind::Loop, Tokenkind::OpenBrace],
            &mut [
                &mut |s| {
                    let b = s.expr_block();
                    s.check(Tokenkind::Semicolon);
                    b
                },
                &mut |s| {
                    let b = s.expr_block();
                    s.check(Tokenkind::Semicolon);
                    b
                },
                &mut |s| {
                    let b = s.expr_block();
                    s.check(Tokenkind::Semicolon);
                    b
                },
            ],
            |s| {
                let e = s.expr_inline();
                s.expect(Tokenkind::Semicolon, &[]);
                e
            },
        )
    }

    fn expr_any(&mut self) -> Expr {
        self.dispatch(
            &[Tokenkind::If, Tokenkind::Loop, Tokenkind::OpenBrace],
            &mut [
                &mut Self::expr_block,
                &mut Self::expr_block,
                &mut Self::expr_block,
            ],
            |s| s.expr_inline(),
        )
    }

    fn expr_block(&mut self) -> Expr {
        let cases = &[Tokenkind::If, Tokenkind::Loop, Tokenkind::OpenBrace];
        self.dispatch(
            cases,
            &mut [
                &mut Self::expr_if,
                &mut Self::expr_loop,
                &mut Self::expr_bareblock,
            ],
            |s| {
                s.unexpected_token(s.peek(), cases, &[]);
                std::process::exit(1);
            },
        )
    }

    fn expr_if(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::If, &[]).loc;
        let cond = self.expr_any();
        let smash = self.expr_block();
        let pass = if self.check(Tokenkind::Else).is_some() {
            self.expr_block()
        } else {
            Expr {
                loc: smash.loc,
                kind: Box::new(expr::Tuple(vec![])).into(),
            }
        };
        let loc = loc.meet(&pass.loc);
        Expr {
            loc,
            kind: Box::new(expr::If { cond, smash, pass }).into(),
        }
    }

    fn expr_loop(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Loop, &[]).loc;
        let body = self.expr_block();
        let loc = loc.meet(&body.loc);
        Expr {
            loc,
            kind: Box::new(expr::Loop(body)).into(),
        }
    }

    fn expr_bareblock(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::OpenBrace, &[]).loc;
        let mut body = Vec::new();
        while !self.next_is(Tokenkind::CloseBrace) {
            let e = self.statement();
            if let Some(t) = self.check(Tokenkind::Semicolon) {
                body.push(Expr {
                    loc: t.loc,
                    kind: Box::new(expr::Const(e)).into(),
                });
            } else {
                body.push(e);
                break;
            }
        }
        let loc = loc.meet(
            &self
                .expect(
                    Tokenkind::CloseBrace,
                    &[message!(note => "Maybe a missing semicolon?")],
                )
                .loc,
        );
        Expr {
            loc,
            kind: Box::new(expr::Bareblock(body)).into(),
        }
    }

    fn expr_inline(&mut self) -> Expr {
        self.expr_op(0)
    }

    fn expr_op(&mut self, prec: u32) -> Expr {
        let mut left = if self.expr_op_left_first() {
            self.expr_op_left()
        } else {
            self.expr_atom()
        };
        while self.expr_op_right_first(prec) {
            left = self.expr_op_right(prec, left);
        }
        left
    }

    fn expr_op_left_first(&mut self) -> bool {
        for o in self.ops.clone().iter() {
            if o.lbp.is_some() {
                continue;
            }
            if self.next_is_text(o.spelling) {
                return true;
            }
        }
        false
    }

    fn expr_op_left(&mut self) -> Expr {
        for o in self.ops.clone().iter() {
            if o.lbp.is_some() {
                continue;
            }
            if let Some(t) = self.check_text(o.spelling) {
                let args = self.expr_op(o.rbp.unwrap().get());
                let path = Rc::new([Ident {
                    name: t.text,
                    loc: t.loc,
                }]);
                return Expr {
                    loc: t.loc.meet(&args.loc),
                    kind: Box::new(expr::Call {
                        func: Expr {
                            loc: t.loc,
                            kind: Box::new(expr::Path(path)).into(),
                        },
                        args,
                    })
                    .into(),
                };
            }
        }
        self.unexpected_token(self.peek(), &[], &[]);
        std::process::exit(1);
    }

    fn expr_op_right_first(&self, prec: u32) -> bool {
        if self.next_is(Tokenkind::OpenParen) {
            return true;
        }
        for o in self.ops.clone().iter() {
            if o.lbp.map_or(true, |lbp| lbp.get() < prec) {
                continue;
            }
            if self.next_is_text(o.spelling) {
                return true;
            }
        }
        false
    }

    fn expr_op_right(&mut self, prec: u32, left: Expr) -> Expr {
        if self.next_is(Tokenkind::OpenParen) {
            let args = self.expr_parenthesised();
            return Expr {
                loc: left.loc.meet(&args.loc),
                kind: Box::new(expr::Call { func: left, args }).into(),
            };
        }
        for o in self.ops.clone().iter() {
            if o.lbp.map_or(true, |lbp| lbp.get() < prec) {
                continue;
            }
            if let Some(t) = self.check_text(o.spelling) {
                let args = if let Some(rbp) = o.rbp {
                    let right = self.expr_op(rbp.get());
                    Expr {
                        loc: left.loc.meet(&right.loc),
                        kind: Box::new(expr::Tuple(vec![left, right])).into(),
                    }
                } else {
                    left
                };
                let path = Rc::new([Ident {
                    name: t.text,
                    loc: t.loc,
                }]);
                return Expr {
                    loc: args.loc.meet(&t.loc),
                    kind: Box::new(expr::Call {
                        func: Expr {
                            loc: t.loc,
                            kind: Box::new(expr::Path(path)).into(),
                        },
                        args,
                    })
                    .into(),
                };
            }
        }
        unreachable!("because loops on expr_op_right_first()");
    }

    fn expr_atom(&mut self) -> Expr {
        use Tokenkind::*;
        let t = self.peek();
        match t.map(|t| t.kind) {
            Some(OpenParen) => self.expr_parenthesised(),
            Some(Name) => {
                let part = self.eat();
                let mut names = vec![Ident {
                    name: part.text,
                    loc: part.loc,
                }];
                let mut loc = part.loc;
                while self.check(Tokenkind::Slash).is_some() {
                    let part = self.expect(Tokenkind::Name, &[]);
                    names.push(Ident {
                        name: part.text,
                        loc: part.loc,
                    });
                    loc = loc.meet(&part.loc);
                }
                Expr {
                    kind: Box::new(expr::Path(names.into())).into(),
                    loc,
                }
            }
            Some(Number) => {
                let t = self.eat();
                Expr {
                    loc: t.loc,
                    kind: Box::new(expr::Number(t.text)).into(),
                }
            }
            Some(String) => {
                let t = self.eat();
                Expr {
                    loc: t.loc,
                    kind: Box::new(expr::String(t.text)).into(),
                }
            }
            Some(Bool) => {
                let t = self.eat();
                Expr {
                    loc: t.loc,
                    kind: Box::new(expr::Bool(t.text)).into(),
                }
            }
            _ => {
                self.unexpected_token(t, &[OpenParen, Name, String, Number, Bool], &[]);
                std::process::exit(1);
            }
        }
    }

    fn expr_parenthesised(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::OpenParen, &[]).loc;
        let mut fields = Vec::new();
        if !self.next_is(Tokenkind::CloseParen) {
            fields.push(self.expr_any());
            if self.check(Tokenkind::Semicolon).is_some() {
                let t = fields.pop().unwrap();
                let n = self.expect(Tokenkind::Number, &[]);
                for _ in 0..n.text.parse::<usize>().unwrap() {
                    fields.push(t.clone());
                }
            }
            while self.check(Tokenkind::Comma).is_some() {
                fields.push(self.expr_any());
                if self.check(Tokenkind::Semicolon).is_some() {
                    let t = fields.pop().unwrap();
                    let n = self.expect(Tokenkind::Number, &[]);
                    for _ in 0..n.text.parse::<usize>().unwrap() {
                        fields.push(t.clone());
                    }
                }
            }
        }
        let loc = loc.meet(&self.expect(Tokenkind::CloseParen, &[]).loc);
        match fields.len() {
            1 => fields.pop().unwrap(),
            _ => Expr {
                loc,
                kind: Box::new(expr::Tuple(fields)).into(),
            },
        }
    }

    fn statement(&mut self) -> Expr {
        self.dispatch(
            &[
                Tokenkind::Let,
                Tokenkind::Mut,
                Tokenkind::Break,
                Tokenkind::Return,
            ],
            &mut [
                &mut Self::statement_let,
                &mut Self::statement_mut,
                &mut Self::statement_break,
                &mut Self::statement_return,
            ],
            |s| {
                if s.n_is(1, Tokenkind::Equals) {
                    s.statement_assign()
                } else {
                    s.expr_any()
                }
            },
        )
    }

    fn statement_let(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Let, &[]).loc;
        let binding = self.binding();
        self.expect(Tokenkind::Equals, &[]);
        let init = self.expr_any();
        let loc = loc.meet(&init.loc);
        Expr {
            loc,
            kind: Box::new(expr::Let(binding, init)).into(),
        }
    }

    fn statement_mut(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Mut, &[]).loc;
        let binding = self.binding();
        self.expect(Tokenkind::Equals, &[]);
        let init = self.expr_any();
        let loc = loc.meet(&init.loc);
        Expr {
            loc,
            kind: Box::new(expr::Mut(binding, init)).into(),
        }
    }

    fn statement_break(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Break, &[]).loc;
        let val = if self.next_is(Tokenkind::Semicolon) {
            None
        } else {
            Some(self.expr_any())
        };
        let loc = loc.meet(val.as_ref().map_or(&loc, |v| &v.loc));
        Expr {
            loc,
            kind: Box::new(expr::Break(val)).into(),
        }
    }

    fn statement_return(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Return, &[]).loc;
        let val = if self.next_is(Tokenkind::Semicolon) {
            None
        } else {
            Some(self.expr_any())
        };
        let loc = loc.meet(val.as_ref().map_or(&loc, |v| &v.loc));
        Expr {
            loc,
            kind: Box::new(expr::Return(val)).into(),
        }
    }

    fn statement_assign(&mut self) -> Expr {
        let name = self.expect(Tokenkind::Name, &[]);
        let loc = name.loc;
        self.expect(Tokenkind::Equals, &[]);
        let val = self.expr_any();
        let loc = loc.meet(&val.loc);
        let name = Ident {
            name: name.text,
            loc: name.loc,
        };
        Expr {
            loc,
            kind: Box::new(expr::Assign(name, val)).into(),
        }
    }

    fn unexpected_token(&self, t: Option<Token<Istr>>, exps: &[Tokenkind], notes: &[FmtMessage]) {
        let spell = t.map_or("EOF", |t| t.kind.spelling());
        let title = match exps.len() {
            0 => format!("Unexpected token '{}'!", spell),
            1 => format!("Expected '{}', but got '{}'!", exps[0].spelling(), spell),
            _ => {
                let mut title = String::from("Expected one of ");
                for e in exps {
                    _ = write!(title, "'{}', ", e.spelling());
                }
                _ = write!(title, "but got '{}'!", spell);
                title
            }
        };
        let loc = t.map_or(self.ctx.eof_loc(self.m), |t| t.loc);
        // self.ctx.report(Message::error(&title, Some(loc)), notes);
        self.ctx.report(message!(error @ loc => "{}", title), notes);
    }
}
