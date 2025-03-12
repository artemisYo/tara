use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::ContextRef,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix},
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::collections::BTreeMap as Map;

use crate::{
    fill,
    misc::{Istr, Ivec},
};

use super::{
    uir::{self, ExprId, LocalVec},
    Tara,
};

#[derive(Clone)]
pub struct Out {
    pub func: FunctionValue<'static>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct In {
    pub i: uir::Id,
}

impl Tara {
    pub fn codegen(&mut self, i: In) -> Out {
        match self.codegen.get(&i) {
            Some(Some(o)) => o.clone(),
            Some(None) => panic!("codegen entered a cycle!"),
            None => {
                self.codegen.insert(i, None);
                let data = codegen(self, i);
                self.codegen.insert(i, Some(data));
                self.codegen.get(&i).cloned().unwrap().unwrap()
            }
        }
    }
}

#[derive(PartialEq, Eq)]
enum Control {
    Plain(AnyValueEnum<'static>),
    Break,
    Return,
}

fn codegen(tara: &mut Tara, i: In) -> Out {
    tara.fill(fill::In { i: i.i });
    let ty = tara.uir_types[tara.uir_items[i.i].typ.kind].clone();
    let llvm_ctx = tara.llvm_mod.get_context();
    let AnyTypeEnum::FunctionType(ty) = lower_type(ty, None, &tara.uir_types, llvm_ctx) else {
        panic!("non-function typed functions??")
    };
    let val = tara
        .llvm_mod
        .add_function(tara.uir_items[i.i].name.name.0, ty, None);
    let entry = llvm_ctx.append_basic_block(val, "entry");
    let ret_b = llvm_ctx.append_basic_block(val, "return");
    let builder = llvm_ctx.create_builder();
    builder.position_at_end(entry);
    let ret_v = builder
        .build_alloca(ty.get_return_type().unwrap(), "&return_slot")
        .unwrap();
    let mut ctx = Context {
        lets: Default::default(),
        loops: Vec::new(),
        ctx: llvm_ctx,
        id: i.i,
        builder,
        ret_v,
        ret_b,
        tara,
        val,
    };
    match ctx.expressions(ctx.tara.uir_items[ctx.id].body) {
        Control::Return => {}
        Control::Break => panic!("breaks in plain bareblock shouldn't happen"),
        Control::Plain(body) => {
            let body = any_to_basic_value(body);
            ctx.builder.build_store(ctx.ret_v, body).unwrap();
            ctx.builder.build_unconditional_branch(ctx.ret_b).unwrap();
        }
    }
    ctx.builder.position_at_end(ctx.ret_b);
    let ret_v = ctx
        .builder
        .build_load(ty.get_return_type().unwrap(), ctx.ret_v, "*return_slot")
        .unwrap();
    ctx.builder.build_return(Some(&ret_v)).unwrap();
    Out { func: val }
}

fn any_to_basic_type(t: AnyTypeEnum<'static>, ctx: ContextRef<'static>) -> BasicTypeEnum<'static> {
    match t {
        AnyTypeEnum::FunctionType(_) => ctx.ptr_type(AddressSpace::from(0u16)).into(),
        AnyTypeEnum::ArrayType(t) => t.into(),
        AnyTypeEnum::FloatType(t) => t.into(),
        AnyTypeEnum::IntType(t) => t.into(),
        AnyTypeEnum::PointerType(t) => t.into(),
        AnyTypeEnum::StructType(t) => t.into(),
        AnyTypeEnum::VectorType(t) => t.into(),
        AnyTypeEnum::VoidType(_) => panic!("I have no idea"),
    }
}

fn any_to_basic_value(v: AnyValueEnum<'static>) -> BasicValueEnum<'static> {
    match v {
        AnyValueEnum::ArrayValue(v) => v.into(),
        AnyValueEnum::IntValue(v) => v.into(),
        AnyValueEnum::FloatValue(v) => v.into(),
        AnyValueEnum::PhiValue(v) => v.as_basic_value(),
        AnyValueEnum::FunctionValue(v) => v.as_global_value().as_pointer_value().into(),
        AnyValueEnum::PointerValue(v) => v.into(),
        AnyValueEnum::StructValue(v) => v.into(),
        AnyValueEnum::VectorValue(v) => v.into(),
        AnyValueEnum::MetadataValue(_) => panic!("I do not know wtf this is"),
        AnyValueEnum::InstructionValue(_) => panic!("what why?"),
    }
}

fn lower_type(
    t: uir::Typekind,
    args: Option<uir::Typekind>,
    types: &Ivec<uir::TypeId, uir::Typekind>,
    ctx: ContextRef<'static>,
) -> AnyTypeEnum<'static> {
    match t {
        uir::Typekind::Func { args, ret } => {
            let args = types[args].clone();
            let args: BasicMetadataTypeEnum = match lower_type(args, None, types, ctx) {
                AnyTypeEnum::FunctionType(_) => ctx.ptr_type(AddressSpace::from(0u16)).into(),
                AnyTypeEnum::ArrayType(t) => t.into(),
                AnyTypeEnum::FloatType(t) => t.into(),
                AnyTypeEnum::IntType(t) => t.into(),
                AnyTypeEnum::PointerType(t) => t.into(),
                AnyTypeEnum::StructType(t) => t.into(),
                AnyTypeEnum::VectorType(t) => t.into(),
                AnyTypeEnum::VoidType(_) => panic!("I have no idea"),
            };
            let ret = types[ret].clone();
            let ret = any_to_basic_type(lower_type(ret, None, types, ctx), ctx);
            ret.fn_type(&[args], false).into()
        }
        uir::Typekind::Call { args, func } => {
            let args = types[args].clone();
            let func = types[func].clone();
            lower_type(func, Some(args), types, ctx)
        }
        uir::Typekind::String => ctx.ptr_type(AddressSpace::from(0u16)).into(),
        uir::Typekind::Bool => ctx.custom_width_int_type(1).into(),
        uir::Typekind::Int => ctx.i64_type().into(),
        uir::Typekind::Tup => {
            let Some(uir::Typekind::Bundle(args)) = args else {
                panic!("uninstantiated tuples??")
            };
            let mut fields = Vec::with_capacity(args.len());
            for &a in args.iter() {
                let a = types[a].clone();
                let a: BasicTypeEnum = match lower_type(a, None, types, ctx) {
                    AnyTypeEnum::FunctionType(_) => ctx.ptr_type(AddressSpace::from(0u16)).into(),
                    AnyTypeEnum::ArrayType(t) => t.into(),
                    AnyTypeEnum::FloatType(t) => t.into(),
                    AnyTypeEnum::IntType(t) => t.into(),
                    AnyTypeEnum::PointerType(t) => t.into(),
                    AnyTypeEnum::StructType(t) => t.into(),
                    AnyTypeEnum::VectorType(t) => t.into(),
                    AnyTypeEnum::VoidType(_) => panic!("I have no idea"),
                };
                fields.push(a);
            }
            ctx.struct_type(fields.as_slice(), false).into()
        }
        uir::Typekind::Recall(_) => todo!("custom types do not exist yet"),
        uir::Typekind::Bundle(_) => panic!("found bundle outside of call!"),
        uir::Typekind::Var(n) => panic!("well something went wrong!\nfound type var: {}", n),
    }
}

struct Context<'a> {
    builder: Builder<'static>,
    ctx: ContextRef<'static>,
    val: FunctionValue<'static>,
    lets: Map<Istr, (BasicTypeEnum<'static>, PointerValue<'static>)>,
    id: uir::Id,
    ret_v: PointerValue<'static>,
    ret_b: BasicBlock<'static>,
    loops: Vec<(BasicBlock<'static>, PointerValue<'static>)>,
    tara: &'a mut Tara,
}

impl Context<'_> {
    fn locals(&self) -> &LocalVec {
        &self.tara.uir_items[self.id].locals
    }
    fn expressions(&mut self, e: ExprId) -> Control {
        match self.locals()[e].kind {
            uir::Exprkind::If { cond, smash, pass } => {
                let cond = match self.expressions(cond) {
                    Control::Plain(AnyValueEnum::IntValue(c)) => c,
                    Control::Plain(_) => panic!("typechecker should've caught this"),
                    e => return e,
                };
                let smash_b = self.ctx.append_basic_block(self.val, "smash");
                let pass_b = self.ctx.append_basic_block(self.val, "pass");
                let post_b = self.ctx.append_basic_block(self.val, "post");
                self.builder
                    .build_conditional_branch(cond, smash_b, pass_b)
                    .unwrap();
                let mut cont = false;

                self.builder.position_at_end(smash_b);
                let smash_v = self.expressions(smash);
                if let Control::Plain(_) = &smash_v {
                    self.builder.build_unconditional_branch(post_b).unwrap();
                    cont = true;
                }
                self.builder.position_at_end(pass_b);
                let pass_v = self.expressions(pass);
                if let Control::Plain(_) = &pass_v {
                    self.builder.build_unconditional_branch(post_b).unwrap();
                    cont = true;
                }

                self.builder.position_at_end(post_b);
                if !cont {
                    self.builder.build_unreachable().unwrap();
                    if Control::Break == smash_v {
                        return Control::Break;
                    }
                    if let Control::Break = &pass_v {
                        return pass_v;
                    }
                    return smash_v;
                }

                let (l, p, q) = match (smash_v, pass_v) {
                    (Control::Plain(p), Control::Plain(q)) => (
                        2,
                        (any_to_basic_value(p), smash_b),
                        (any_to_basic_value(q), pass_b),
                    ),
                    (Control::Plain(p), _) => (
                        1,
                        (any_to_basic_value(p), smash_b),
                        (self.ctx.const_struct(&[], false).into(), pass_b),
                    ),
                    (_, Control::Plain(p)) => (
                        1,
                        (any_to_basic_value(p), pass_b),
                        (self.ctx.const_struct(&[], false).into(), smash_b),
                    ),
                    _ => unreachable!(),
                };
                let incoming: &[(&dyn BasicValue, _)] = if l == 1 {
                    &[(&p.0, p.1)]
                } else {
                    &[(&p.0, p.1), (&q.0, q.1)]
                };

                let phi_v = self
                    .builder
                    .build_phi(p.0.get_type(), "merge_branches")
                    .unwrap();
                phi_v.add_incoming(incoming);
                Control::Plain(phi_v.into())
            }
            uir::Exprkind::Call { func, args } => {
                let func_v = match self.expressions(func) {
                    Control::Plain(AnyValueEnum::FunctionValue(v)) => v,
                    Control::Plain(_) => panic!("help me"),
                    e => return e,
                };
                let args = match self.expressions(args) {
                    Control::Plain(v) => v,
                    e => return e,
                };
                let res = self
                    .builder
                    .build_call(func_v, &[any_to_basic_value(args).into()], "call")
                    .unwrap();
                Control::Plain(res.as_any_value_enum())
            }
            uir::Exprkind::Tuple(ref expr_ids) => {
                let expr_ids = expr_ids.clone();
                let ty = self.tara.uir_types[self.locals()[e].typ.kind].clone();
                let AnyTypeEnum::StructType(ty) =
                    lower_type(ty, None, &self.tara.uir_types, self.ctx)
                else {
                    panic!("aaaaaaaa")
                };
                let mut comma = ty.get_undef().into();
                for (ix, &id) in expr_ids.iter().enumerate() {
                    let v = match self.expressions(id) {
                        Control::Plain(v) => any_to_basic_value(v),
                        e => return e,
                    };
                    comma = self.builder.build_insert_value(comma, v, ix as u32, "tuple").unwrap();
                }
                Control::Plain(comma.as_any_value_enum())
            }
            uir::Exprkind::Loop(expr_id) => {
                let ty = self.locals()[e].typ.kind;
                let ty = self.tara.uir_types[ty].clone();
                let ty = lower_type(ty, None, &self.tara.uir_types, self.ctx);
                let ty = any_to_basic_type(ty, self.ctx);
                let post_v = self.builder.build_alloca(ty, "&loop_slot").unwrap();
                let loop_b = self.ctx.append_basic_block(self.val, "loop");
                let post_b = self.ctx.append_basic_block(self.val, "loop_end");
                self.loops.push((post_b, post_v));
                self.builder.build_unconditional_branch(loop_b).unwrap();
                self.builder.position_at_end(loop_b);
                let v = self.expressions(expr_id);
                if Control::Return == v {
                    return Control::Return;
                }
                if let Control::Plain(_) = &v {
                    self.builder.build_unconditional_branch(loop_b).unwrap();
                }
                self.builder.position_at_end(post_b);
                self.loops.pop();
                let v = self.builder.build_load(ty, post_v, "*loop_slot").unwrap();
                Control::Plain(v.into())
            }
            uir::Exprkind::Bareblock(ref expr_ids) => {
                let mut out = self.ctx.const_struct(&[], false).into();
                let expr_ids = expr_ids.clone();
                for &id in expr_ids.iter() {
                    match self.expressions(id) {
                        Control::Plain(v) => out = v,
                        e => return e,
                    }
                }
                Control::Plain(out)
            }
            // name resolution done before the typer is useless now
            // as the data we need is not a field in the struct
            uir::Exprkind::Recall(Ok(l)) => {
                let uir::Bindkind::Name(n, _) = self.locals()[l].kind else {
                    panic!("I have no idea")
                };
                let (ty, ptr) = self.lets.get(&n).copied().unwrap();
                let out = self.builder.build_load(ty, ptr, "recall_local").unwrap();
                Control::Plain(out.into())
            }
            // 'cycles' like /this/ are fine, the value exists already
            uir::Exprkind::Recall(Err(g)) => Control::Plain(if self.id == g {
                self.val.into()
            } else {
                self.tara.codegen(In { i: g }).func.into()
            }),

            uir::Exprkind::Number(istr) => {
                let v = self
                    .ctx
                    .custom_width_int_type(64)
                    .const_int_from_string(istr.0, StringRadix::Decimal)
                    .unwrap();
                Control::Plain(v.into())
            }
            uir::Exprkind::String(istr) => {
                let global = self
                    .builder
                    .build_global_string_ptr(istr.0, "string_literal")
                    .unwrap();
                Control::Plain(global.as_pointer_value().into())
            }
            uir::Exprkind::Bool(istr) => {
                let v = if istr == "true".into() { 1 } else { 0 };
                let v = self.ctx.custom_width_int_type(1).const_int(v, false);
                Control::Plain(v.into())
            }
            uir::Exprkind::Arguments => Control::Plain(self.val.get_nth_param(0).unwrap().into()),
            uir::Exprkind::Poison => {
                panic!("Poison should've barred codegen from running earlier on!")
            }
            uir::Exprkind::Let(binding_id, expr_id) => {
                let v = match self.expressions(expr_id) {
                    Control::Plain(v) => v,
                    e => return e,
                };
                let v = any_to_basic_value(v);
                self.bindings(binding_id, v);
                Control::Plain(self.ctx.const_struct(&[], false).into())
            }
            uir::Exprkind::Assign(l, expr_id) => {
                let uir::Bindkind::Name(n, _) = self.locals()[l].kind else {
                    panic!("I have no idea")
                };
                let v = match self.expressions(expr_id) {
                    Control::Plain(v) => v,
                    e => return e,
                };
                let (_, ptr) = self.lets.get(&n).unwrap();
                self.builder
                    .build_store(*ptr, any_to_basic_value(v))
                    .unwrap();
                Control::Plain(self.ctx.const_struct(&[], false).into())
            }
            uir::Exprkind::Break { val, target: _ } => {
                let v = match self.expressions(val) {
                    Control::Plain(v) => v,
                    e => return e,
                };
                let (block, ptr) = self.loops.last().unwrap();
                self.builder
                    .build_store(*ptr, any_to_basic_value(v))
                    .unwrap();
                self.builder.build_unconditional_branch(*block).unwrap();
                Control::Break
            }
            uir::Exprkind::Return(expr_id) => {
                let v = match self.expressions(expr_id) {
                    Control::Plain(v) => v,
                    e => return e,
                };
                self.builder
                    .build_store(self.ret_v, any_to_basic_value(v))
                    .unwrap();
                self.builder.build_unconditional_branch(self.ret_b).unwrap();
                Control::Return
            }
            uir::Exprkind::Const(expr_id) => {
                match self.expressions(expr_id) {
                    Control::Plain(_) => {}
                    e => return e,
                }
                Control::Plain(self.ctx.const_struct(&[], false).into())
            }
        }
    }

    fn bindings(&mut self, b: uir::BindingId, v: BasicValueEnum<'static>) {
        match self.locals()[b].kind {
            uir::Bindkind::Empty => {},
            uir::Bindkind::Name(istr, _) => {
                let ty = v.get_type();
                let ptr = self.builder.build_alloca(ty, "let").unwrap();
                self.builder.build_store(ptr, v).unwrap();
                self.lets.insert(istr, (ty, ptr));
            }
            uir::Bindkind::Tuple(ref binding_ids) => {
                let binding_ids = binding_ids.clone();
                let BasicValueEnum::StructValue(v) = v else {
                    panic!("typer shouldn't allow anything else")
                };
                for (i, &b) in binding_ids.iter().enumerate() {
                    let v = self
                        .builder
                        .build_extract_value(v, i as u32, "destructure")
                        .unwrap();
                    self.bindings(b, v);
                }
            }
        }
    }
}
