use super::*;
use crate::scoped_map::ScopeMap;

use cranelift::prelude::*;
use cranelift::codegen;
use cranelift::frontend;
use codegen::{verifier::verify_function, ir::{UserFuncName, function::Function, entities::Value}};

pub fn generate(file: File) -> Function {
    let mut sig = Signature::new(isa::CallConv::SystemV);
    let mut fb_ctx = FunctionBuilderContext::new();
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
    let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);
    let mut ctx = Context::new(builder);
    file.generate(&mut ctx);
    ctx.builder.ins().return_(&[]);
    ctx.builder.finalize();
    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);
    if let Err(e) = res {
        panic!("{}", e);
    }
    func
}

struct Context<'a> {
    builder: FunctionBuilder<'a>,
    decls: ScopeMap<String, Variable>,
}
impl<'a> Context<'a> {
    fn new(builder: FunctionBuilder<'a>) -> Self {
        Self {
            builder,
            decls: ScopeMap::new(),
        }
    }
}

impl File {
    fn generate(self, ctx: &mut Context) {
        let b = ctx.builder.create_block();
        ctx.builder.append_block_params_for_function_params(b);
        ctx.builder.switch_to_block(b);
        ctx.builder.seal_block(b);
        self.body.into_iter().for_each(|s| s.generate(ctx));
        self.tail.generate(ctx);
    }
}

impl Statement {
    fn generate(self, ctx: &mut Context) {
        match self {
            Self::Let(l) => l.generate(ctx),
            Self::Expr(e) => { e.generate(ctx); },
        }
    }
}

impl LetStmt {
    fn generate(self, ctx: &mut Context) {
        let v = Variable::new(ctx.decls.len());
        ctx.builder.declare_var(v, types::I64);
        let res = self.init.generate(ctx);
        ctx.builder.def_var(v, res);
        ctx.decls.insert(self.name, v);
    }
}

impl Expr {
    fn generate(self, ctx: &mut Context) -> Value {
        match self {
            Self::Binary(b) => b.generate(ctx),
            Self::Single(s) => s.generate(ctx),
            Self::While(w) => w.generate(ctx),
            Self::If(i) => i.generate(ctx),
        }
    }
}

impl IfExpr {
    fn generate(self, ctx: &mut Context) -> Value {
        todo!()
    }
}

impl WhileExpr {
    fn generate(self, ctx: &mut Context) -> Value {
        todo!()
    }
}

impl BinExpr {
    fn generate(self, ctx: &mut Context) -> Value {
        let [lhs, rhs] = self.args;
        let lhs = lhs.generate(ctx);
        let rhs = rhs.generate(ctx);
        match self.op {
            BinOp::Plus => ctx.builder.ins().iadd(lhs, rhs),
            BinOp::Minus => ctx.builder.ins().isub(lhs, rhs),
            BinOp::Star => ctx.builder.ins().imul(lhs, rhs),
            BinOp::Slash => ctx.builder.ins().sdiv(lhs, rhs),
        }
    }
}

impl SinExpr {
    fn generate(self, ctx: &mut Context) -> Value {
        match self {
            Self::Number(i) => ctx.builder.ins().iconst(types::I64, i as i64),
            Self::Name(s) => ctx.builder.use_var(*ctx.decls.get(&s).unwrap()),
            Self::Empty => ctx.builder.ins().null(types::INVALID)
        }
    }
}
