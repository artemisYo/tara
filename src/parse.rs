use std::fmt::Write;
use std::{io::BufReader, rc::Rc};

use crate::{
    control::{self, Query},
    data::{
        ast::*,
        files::{self, Files},
        Ident,
    },
    lexer::Tokenizer,
    message,
    tokens::{Token, Tokenkind},
    FmtMessage, Provenance,
};

pub type Map = control::Qmap<files::Id, Rc<Ast>, Ast>;
impl Query<files::Id, Rc<Ast>> for Ast {
    type Inputs<'a> = (&'a Files,);

    fn query(_: &mut Map, &file: &files::Id, (files,): Self::Inputs<'_>) -> Rc<Ast> {
        files[file].source.access(|src| {
            let lexer = Tokenizer::new(file, BufReader::new(src));
            let lexer = lexer.filter(|t| t.kind != Tokenkind::Comment);
            let lexer = Strcat::new(lexer).peekable();

            let mut parser = Parser {
                srcs: files,
                module: file,
                tokens: lexer,
                ops: OPS,
            };
            let ast = parser.top();
            Rc::new(ast)
        })
    }
}

const OPS: &[Op] = {
    use std::num::NonZero;
    &[
        Op {
            spelling: "<",
            lbp: NonZero::new(1),
            rbp: NonZero::new(1),
        },
        Op {
            spelling: "<=",
            lbp: NonZero::new(1),
            rbp: NonZero::new(1),
        },
        Op {
            spelling: ">",
            lbp: NonZero::new(1),
            rbp: NonZero::new(1),
        },
        Op {
            spelling: ">=",
            lbp: NonZero::new(1),
            rbp: NonZero::new(1),
        },
        Op {
            spelling: "==",
            lbp: NonZero::new(1),
            rbp: NonZero::new(1),
        },
        Op {
            spelling: "&",
            lbp: NonZero::new(3),
            rbp: NonZero::new(2),
        },
        Op {
            spelling: "|",
            lbp: NonZero::new(3),
            rbp: NonZero::new(2),
        },
        Op {
            spelling: "<<",
            lbp: NonZero::new(3),
            rbp: NonZero::new(2),
        },
        Op {
            spelling: ">>",
            lbp: NonZero::new(3),
            rbp: NonZero::new(2),
        },
        Op {
            spelling: "+",
            lbp: NonZero::new(4),
            rbp: NonZero::new(5),
        },
        Op {
            spelling: "-",
            lbp: NonZero::new(4),
            rbp: NonZero::new(5),
        },
        Op {
            spelling: "~",
            lbp: None,
            rbp: NonZero::new(6),
        },
    ]
};

struct Parser<'a, L: Iterator<Item = Token<&'static str>>> {
    srcs: &'a Files,
    module: files::Id,
    tokens: std::iter::Peekable<Strcat<L, L::Item>>,
    ops: &'a [Op],
}
impl<L: Iterator<Item = Token<&'static str>>> Parser<'_, L> {
    fn dispatch<T>(
        &mut self,
        cases: &[Tokenkind],
        body: &mut [&mut dyn FnMut(&mut Self) -> T],
        default: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let Some(t) = self.peek() else {
            return default(self);
        };
        for (i, &k) in cases.iter().enumerate() {
            if k == t.kind {
                return body[i](self);
            }
        }
        default(self)
    }

    #[inline]
    fn peek(&mut self) -> Option<Token<&'static str>> {
        self.tokens.peek().copied()
    }

    #[inline]
    fn next_is(&mut self, k: Tokenkind) -> bool {
        self.peek().is_some_and(|t| t.kind == k)
    }
    #[inline]
    fn next_is_any(&mut self, ks: &[Tokenkind]) -> bool {
        self.peek().is_some_and(|t| ks.contains(&t.kind))
    }
    #[inline]
    fn next_is_text(&mut self, s: &'static str) -> bool {
        self.peek().is_some_and(|t| t.text == s)
    }

    #[inline]
    fn eat(&mut self) -> Token<&'static str> {
        let Some(&out) = self.tokens.peek() else {
            self.unexpected_token(None, &[], &[]);
            #[cfg(debug_assertions)]
            let _ = dbg!(std::backtrace::Backtrace::force_capture());
            std::process::exit(1);
        };
        self.tokens.next();
        out
    }
    #[inline]
    fn expect(&mut self, k: Tokenkind, notes: &[FmtMessage]) -> Token<&'static str> {
        if !self.next_is(k) {
            let next = self.peek();
            self.unexpected_token(next, &[k], notes);
            #[cfg(debug_assertions)]
            let _ = dbg!(std::backtrace::Backtrace::force_capture());
            std::process::exit(1);
        }
        self.eat()
    }
    #[inline]
    fn expect_any(&mut self, ks: &[Tokenkind], notes: &[FmtMessage]) -> Token<&'static str> {
        if !self.next_is_any(ks) {
            let next = self.peek();
            self.unexpected_token(next, ks, notes);
            #[cfg(debug_assertions)]
            let _ = dbg!(std::backtrace::Backtrace::force_capture());
            std::process::exit(1);
        }
        self.eat()
    }
    #[inline]
    fn avoid(&mut self, k: Tokenkind, notes: &[FmtMessage]) {
        if let Some(t) = self.check(k) {
            let spell = k.spelling();
            self.srcs.report(
                message!(error @ t.loc => "Unexpected token '{}'!", spell),
                notes,
            );
            #[cfg(debug_assertions)]
            let _ = dbg!(std::backtrace::Backtrace::force_capture());
            std::process::exit(1);
        }
    }

    #[inline]
    fn check(&mut self, k: Tokenkind) -> Option<Token<&'static str>> {
        if !self.next_is(k) {
            None
        } else {
            Some(self.eat())
        }
    }
    #[inline]
    fn check_text(&mut self, t: &'static str) -> Option<Token<&'static str>> {
        if !self.next_is_text(t) {
            None
        } else {
            Some(self.eat())
        }
    }

    fn top(&mut self) -> Ast {
        let mut ast = Ast {
            funcs: Vec::new(),
            types: Vec::new(),
            imports: Vec::new(),
        };
        while self.tokens.peek().is_some() {
            self.decls(&mut ast);
        }
        ast
    }
    fn decls(&mut self, ast: &mut Ast) {
        let cases = &[Tokenkind::Func, Tokenkind::Type, Tokenkind::Import];
        match self.dispatch(
            cases,
            &mut [
                &mut |s| Ok(s.function()),
                &mut |s| Err(Ok(s.decls_type())),
                &mut |s| Err(Err(s.import())),
            ],
            |s| {
                let t = s.peek();
                s.unexpected_token(t, cases, &[]);
                std::process::exit(1)
            },
        ) {
            Ok(f) => ast.funcs.push(f),
            Err(Ok(t)) => ast.types.push(t),
            Err(Err(i)) => ast.imports.push(i),
        }
    }

    fn import(&mut self) -> Import {
        let loc = self.expect(Tokenkind::Import, &[]).loc;
        let part = self
            .check(Tokenkind::Name)
            .unwrap_or_else(|| self.expect(Tokenkind::Return, &[]));
        let mut path: Vec<Ident> = vec![part.into()];
        while self.check(Tokenkind::Slash).is_some() {
            let part = self.expect_any(
                &[Tokenkind::Name, Tokenkind::Return, Tokenkind::Ellipsis],
                &[],
            );
            path.push(part.into());
            if part.kind == Tokenkind::Ellipsis {
                break;
            }
        }
        let loc = self.expect(Tokenkind::Semicolon, &[]).loc.meet(&loc);
        Import { path, loc }
    }

    fn decls_type(&mut self) -> Typedecl {
        // 'type' name
        //   { '=' variant
        //   / body }
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
        // '{' variant '}'
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
        // 'case' name '(' ... ')' ';'
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

    // fn binding_constructor(&mut self) -> Binding {
    //     let (path, loc) = self.path();
    //     let fields = self.binding();
    //     let whole_loc = loc.meet(&fields.loc);
    //     Binding {
    //         kind: Box::new(binding::Constructor {
    //             constructor_loc: loc,
    //             constructor: path,
    //             fields,
    //         })
    //         .into(),
    //         loc: whole_loc,
    //     }
    // }

    fn binding_name(&mut self) -> Binding {
        let part = self.expect(Tokenkind::Name, &[]);
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
            loc = part.loc.meet(&loc);
        }
        let constructor_loc = loc;

        let fields = if self.next_is(Tokenkind::OpenParen) {
            Some(self.binding_parenthesised())
        } else {
            None
        };

        let typ = if self.check(Tokenkind::Colon).is_some() {
            let t = self.type_();
            loc = loc.meet(&t.loc);
            Some(t)
        } else {
            None
        };

        Binding {
            loc,
            kind: match (names.len(), fields) {
                (_, Some(fields)) => Box::new(binding::Constructor {
                    constructor_loc,
                    constructor: names,
                    fields,
                })
                .into(),
                // as all constructors have atleast an empty fields binding
                (_, None) => binding::Name(names[0].name, typ).into(),
            },
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
        for o in self.ops.iter() {
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
        for o in self.ops.iter() {
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
                            kind: Typekind::Recall(vec![t.into()]),
                        }),
                    },
                };
            }
        }
        let fin = self.peek();
        self.unexpected_token(fin, &[], &[]);
        std::process::exit(1);
    }

    fn type_op_right_first(&mut self, prec: u32) -> bool {
        if self.next_is(Tokenkind::OpenParen) {
            return true;
        }
        for o in self.ops.iter() {
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
        for o in self.ops.iter() {
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
                            kind: Typekind::Recall(vec![t.into()]),
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
            let (path, loc) = self.path();
            Type {
                kind: Typekind::Recall(path),
                loc,
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
                let t = s.peek();
                s.unexpected_token(t, cases, &[]);
                std::process::exit(1);
            },
        )
    }

    fn expr_if(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::If, &[]).loc;
        let cond = self.expr_any();
        self.avoid(Tokenkind::If, &[
            message!(note => "If-expressions may not contain another if-expression without indirection!"),
            message!(note => "Parenthesise the inner if-expression!"),
            ]);
        let smash = if self.check(Tokenkind::Equals).is_some() {
            self.expr_any()
        } else {
            self.expr_block()
        };
        let pass = if self.check(Tokenkind::Else).is_some() {
            self.expr_any()
        } else {
            Expr {
                loc: smash.loc,
                kind: Rc::new(expr::Tuple(vec![])).into(),
            }
        };
        let loc = loc.meet(&pass.loc);
        Expr {
            loc,
            kind: Rc::new(expr::If { cond, smash, pass }).into(),
        }
    }

    fn expr_loop(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Loop, &[]).loc;
        let body = self.expr_block();
        let loc = loc.meet(&body.loc);
        Expr {
            loc,
            kind: Rc::new(expr::Loop(body)).into(),
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
                    kind: Rc::new(expr::Const(e)).into(),
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
            kind: Rc::new(expr::Bareblock(body)).into(),
        }
    }

    fn expr_inline(&mut self) -> Expr {
        let left = self.expr_op(0);
        if self.check(Tokenkind::Equals).is_some() {
            let right = self.expr_any();
            let loc = left.loc.meet(&right.loc);
            Expr {
                kind: Rc::new(expr::Assign(left, right)).into(),
                loc,
            }
        } else {
            left
        }
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
        for o in self.ops.iter() {
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
        for o in self.ops.iter() {
            if o.lbp.is_some() {
                continue;
            }
            if let Some(t) = self.check_text(o.spelling) {
                let args = self.expr_op(o.rbp.unwrap().get());
                let path = vec![Ident {
                    name: t.text,
                    loc: t.loc,
                }];
                return Expr {
                    loc: t.loc.meet(&args.loc),
                    kind: Rc::new(expr::Call {
                        func: Expr {
                            loc: t.loc,
                            kind: Rc::new(expr::Path(path)).into(),
                        },
                        args,
                    })
                    .into(),
                };
            }
        }
        let fin = self.peek();
        self.unexpected_token(fin, &[], &[]);
        std::process::exit(1);
    }

    fn expr_op_right_first(&mut self, prec: u32) -> bool {
        if self.next_is(Tokenkind::OpenParen) {
            return true;
        }
        for o in self.ops.iter() {
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
                kind: Rc::new(expr::Call { func: left, args }).into(),
            };
        }
        for o in self.ops.iter() {
            if o.lbp.map_or(true, |lbp| lbp.get() < prec) {
                continue;
            }
            if let Some(t) = self.check_text(o.spelling) {
                let args = if let Some(rbp) = o.rbp {
                    let right = self.expr_op(rbp.get());
                    Expr {
                        loc: left.loc.meet(&right.loc),
                        kind: Rc::new(expr::Tuple(vec![left, right])).into(),
                    }
                } else {
                    left
                };
                let path = vec![Ident {
                    name: t.text,
                    loc: t.loc,
                }];
                return Expr {
                    loc: args.loc.meet(&t.loc),
                    kind: Rc::new(expr::Call {
                        func: Expr {
                            loc: t.loc,
                            kind: Rc::new(expr::Path(path)).into(),
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
            Some(Let) => self.expr_let(),
            Some(Mut) => self.expr_mut(),
            Some(Name) => {
                let (names, loc) = self.path();
                Expr {
                    kind: Rc::new(expr::Path(names)).into(),
                    loc,
                }
            }
            Some(Number) => {
                let t = self.eat();
                Expr {
                    loc: t.loc,
                    kind: Rc::new(expr::Number(t.text)).into(),
                }
            }
            Some(String) => {
                let t = self.eat();
                Expr {
                    loc: t.loc,
                    kind: Rc::new(expr::String(t.text)).into(),
                }
            }
            Some(Bool) => {
                let t = self.eat();
                Expr {
                    loc: t.loc,
                    kind: Rc::new(expr::Bool(t.text)).into(),
                }
            }
            _ => {
                self.unexpected_token(t, &[OpenParen, Name, String, Number, Bool], &[]);
                std::process::exit(1);
            }
        }
    }

    fn path(&mut self) -> (Vec<Ident>, Provenance) {
        let part = self.expect(Tokenkind::Name, &[]);
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
        (names, loc)
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
                kind: Rc::new(expr::Tuple(fields)).into(),
            },
        }
    }

    fn expr_let(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Let, &[]).loc;
        let binding = self.binding();
        self.expect(Tokenkind::Equals, &[]);
        let init = self.expr_any();
        let loc = loc.meet(&init.loc);
        Expr {
            loc,
            kind: Rc::new(expr::Let(binding, init)).into(),
        }
    }

    fn expr_mut(&mut self) -> Expr {
        let loc = self.expect(Tokenkind::Mut, &[]).loc;
        let binding = self.binding();
        self.expect(Tokenkind::Equals, &[]);
        let init = self.expr_any();
        let loc = loc.meet(&init.loc);
        Expr {
            loc,
            kind: Rc::new(expr::Mut(binding, init)).into(),
        }
    }

    fn statement(&mut self) -> Expr {
        self.dispatch(
            &[Tokenkind::Break, Tokenkind::Return],
            &mut [&mut Self::statement_break, &mut Self::statement_return],
            |s| s.expr_any(),
            // if s.n_is(1, Tokenkind::Equals) {
            //     s.statement_assign()
            // } else {
            //     s.expr_any()
            // }
        )
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
            kind: Rc::new(expr::Break(val)).into(),
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
            kind: Rc::new(expr::Return(val)).into(),
        }
    }

    // fn statement_assign(&mut self) -> Expr {
    //     let name = self.expect(Tokenkind::Name, &[]);
    //     let loc = name.loc;
    //     self.expect(Tokenkind::Equals, &[]);
    //     let val = self.expr_any();
    //     let loc = loc.meet(&val.loc);
    //     let name = Ident {
    //         name: name.text,
    //         loc: name.loc,
    //     };
    //     Expr {
    //         loc,
    //         kind: Rc::new(expr::Assign(name, val)).into(),
    //     }
    // }

    fn unexpected_token(&self, t: Option<Token<&str>>, exps: &[Tokenkind], notes: &[FmtMessage]) {
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
        let loc = t.map_or(self.srcs.eof_loc(self.module), |t| t.loc);
        self.srcs
            .report(message!(error @ loc => "{}", title), notes);
    }
}

impl Type {
    fn unit(loc: Provenance) -> Self {
        Type {
            kind: Typekind::Call {
                args: Box::new(Type {
                    kind: Typekind::Bundle(Vec::new()),
                    loc,
                }),
                func: Box::new(Type {
                    kind: Typekind::Recall(vec![Ident { name: "tuple", loc }]),
                    loc,
                }),
            },
            loc,
        }
    }
}

impl From<Token<&'static str>> for Ident {
    fn from(t: Token<&'static str>) -> Self {
        Self {
            name: t.text,
            loc: t.loc,
        }
    }
}

struct Strcat<L: Iterator<Item = I>, I>(std::iter::Peekable<L>);
impl<L: Iterator<Item = Token<&'static str>>> Strcat<L, L::Item> {
    fn new(l: L) -> Self {
        Self(l.peekable())
    }
}
impl<L: Iterator<Item = Token<&'static str>>> Iterator for Strcat<L, L::Item> {
    type Item = Token<&'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        fn cat(b: &mut String, s: &str) {
            let s = s
                .strip_prefix('"')
                .expect("strings should always be quoted");
            b.reserve(s.len());
            let iter = s.chars();
            let iter = iter.scan(false, |state, elem| {
                let current = *state;
                *state = elem == '\\' && !*state;
                Some((current, elem))
            });
            for c in iter {
                match c {
                    (false, '"') => break,
                    (false, '\\') => {}
                    (false, '\n') => {
                        b.push('\n');
                        break;
                    }
                    (true, '\n') => break,
                    (true, '0') => b.push('\0'),
                    (true, 'n') => b.push('\n'),
                    (true, 't') => b.push('\t'),
                    (_, c) => b.push(c),
                }
            }
        }

        let t = self.0.next()?;
        if t.kind != Tokenkind::String {
            return Some(t);
        }
        let mut text = String::new();
        let mut loc = t.loc;
        cat(&mut text, t.text);
        while let Some(&t) = self.0.peek().filter(|t| t.kind == Tokenkind::String) {
            self.0.next();
            loc = t.loc.meet(&loc);
            cat(&mut text, t.text);
        }

        Some(Token {
            kind: Tokenkind::String,
            text: text.leak(),
            loc,
        })
    }
}
