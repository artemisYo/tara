use crate::scoped_map::*;

use super::*;

pub fn run(ast: Root) -> Value {
    ast.run(&mut Ctx::new())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
	Int(isize),
	Void,
}
impl Value {
	fn unwrap_num(&self) -> isize {
		match self {
			Self::Int(i) => *i,
			_ => panic!()
		}
	}
}

struct Ctx<'a> {
    decls: ScopeMap<&'a str, Value>,
}
impl<'a> Ctx<'a> {
    fn new() -> Self {
        Self {
            decls: ScopeMap::new(),
        }
    }
}

impl File {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        self.body.iter().for_each(|s| s.run(ctx));
        self.tail.as_ref().run(ctx)
    }
}

impl Statement {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) {
        match self {
            Self::Expr(_) => {} // no effects yet
            Self::Let(l) => l.run(ctx),
        }
    }
}

impl LetStmt {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) {
        let val = self.init.run(ctx);
        ctx.decls.insert(&self.name, val);
    }
}

impl Expr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        match self {
            Expr::Binary(b) => b.run(ctx),
            Expr::Single(s) => s.run(ctx),
            Expr::If(i) => i.run(ctx),
            Expr::While(w) => w.run(ctx),
        }
    }
}

impl Block {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        let scope = ctx.decls.scope();
        let out = self.0.run(ctx);
        ctx.decls.restore(scope);
        out
    }
    fn run_no_scope<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        self.0.run(ctx)
    }
}

impl IfExpr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        let cond = self.cond.run(ctx);
        if self.typing == Some(Type::Void) {
            return Value::Void;
        }
        if cond != Value::Int(0) {
            self.smash.run(ctx)
        } else {
            self.pass.run(ctx)
        }
    }
}

impl WhileExpr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        Value::Void
    }
}

impl BinExpr {
    fn run<'a>(&'a self, ctx: &mut Ctx<'a>) -> Value {
        let [lhs, rhs] = &self.args;
        let lhs = lhs.run(ctx).unwrap_num();
        let rhs = rhs.run(ctx).unwrap_num();
        Value::Int(match self.op {
            BinOp::Plus => lhs + rhs,
            BinOp::Minus => lhs - rhs,
            BinOp::Star => lhs * rhs,
            BinOp::Slash => lhs / rhs,
        })
    }
}
impl SinExpr {
    fn run(&self, ctx: &mut Ctx) -> Value {
        match self {
            Self::Number(n) => Value::Int(*n),
            Self::Name(n) => ctx.decls.get(&n.as_str()).expect("use of undeclared name!").clone(),
            Self::Empty => Value::Void
        }
    }
}
