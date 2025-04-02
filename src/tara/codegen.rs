use either::Either::{Left, Right};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::ContextRef,
    types::{
        AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix, StructType,
    },
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, InlineAsmDialect, IntPredicate,
};
use std::{collections::BTreeMap as Map, rc::Rc};

use crate::{
    fill, gen_query,
    misc::{CheapClone, Istr},
};

use super::{
    quir::{
        self, binding, expr, Binding, BindingId, Expr, ExprId, FunctionId, Type, TypecaseId,
        TypedeclId, Typekind,
    },
    Tara,
};

#[derive(Clone)]
pub enum Out {
    Typedecl(StructType<'static>, Rc<[StructType<'static>]>),
    Func(FunctionValue<'static>),
    Namespace(Rc<[Out]>),
}
impl CheapClone for Out {}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct In {
    pub i: quir::Id,
}

#[derive(PartialEq, Eq)]
pub enum Control {
    Break,
    Return,
}

pub type Result = std::result::Result<AnyValueEnum<'static>, Control>;

gen_query!(codegen);
fn codegen(tara: &mut Tara, In { i }: In) -> Out {
    tara.fill(fill::In { i });
    match tara.quir_items[i] {
        quir::Item::Typecase(_) => typecases(i.into_typecase(), tara),
        quir::Item::Typedecl(_) => typedecls(i.into_typedecl(), tara),
        quir::Item::Function(_) => functions(i.into_func(), tara),
        quir::Item::Namespace(ref n) => {
            let is = n.items.cheap();
            let mut items = Vec::with_capacity(is.len());
            for &i in is.values() {
                items.push(tara.codegen(In { i }));
            }
            Out::Namespace(items.into())
        }
    }
}

fn typedecls(d: TypedeclId, tara: &mut Tara) -> Out {
    let ctx = tara.llvm_mod.get_context();
    let cases = tara.quir_items[d].cases.cheap();
    let tag_size = cases.len().next_power_of_two().ilog2();
    let tag_type = ctx.custom_width_int_type(tag_size);
    let tag_align = tara.target.get_target_data().get_abi_alignment(&tag_type);

    let cases_tails: Vec<_> = cases
        .iter()
        .map(|&c| {
            let bind = tara.quir_items[c].binding;
            let bind = tara.quir_items[c].locals[bind].typ.kind;
            let bind = tara.quir_types[bind].cheap();
            any_to_basic_type(lower_type(bind, None, tara, ctx), ctx)
        })
        .collect();
    let tail_size = cases_tails
        .iter()
        .map(|t| tara.target.get_target_data().get_abi_size(t))
        .max()
        .unwrap_or(0)
        .next_multiple_of(tag_align as u64);

    let opaque_type = ctx.struct_type(
        &[
            ctx.custom_width_int_type(tail_size as u32 * 8).into(),
            tag_type.into(),
        ],
        false,
    );
    let tail_types: Vec<_> = cases_tails
        .into_iter()
        .map(|tail| {
            let padding = tail_size - tara.target.get_target_data().get_abi_size(&tail);
            ctx.struct_type(
                &[
                    tail,
                    ctx.custom_width_int_type(padding as u32 * 8).into(),
                    tag_type.into(),
                ],
                false,
            )
        })
        .collect();

    Out::Typedecl(opaque_type, tail_types.into())
}

// |                i36                 | i5  |
// | ?????????????? tail ?????????????? | tag |
//    ↓
// |   i11    |  i7  |   i9   |   i9    | i5  |
// | concrete | tail | fields | padding | tag |
fn typecases(c: TypecaseId, tara: &mut Tara) -> Out {
    let (case_typ, case_parent, case_index, name) = {
        let case = &tara.quir_items[c];
        (case.typ, case.parent, case.index, case.name.name.0)
    };
    let ty = tara.quir_types[case_typ.kind].cheap();
    let llvm_ctx = tara.llvm_mod.get_context();
    let AnyTypeEnum::FunctionType(ty) = lower_type(ty, None, tara, llvm_ctx) else {
        panic!("constructors should be functions?")
    };
    let (ret, agg_type) = match tara.codegen(In {
        i: case_parent.into(),
    }) {
        Out::Typedecl(ret, cases_type) => (ret, cases_type[case_index]),
        _ => panic!(),
    };

    let val = tara.llvm_mod.add_function(name, ty, None);
    let entry = llvm_ctx.append_basic_block(val, "entry");
    let builder = llvm_ctx.create_builder();
    builder.position_at_end(entry);

    let agg = agg_type.get_undef();
    let bind = val.get_first_param().unwrap();
    let agg = builder
        .build_insert_value(agg, bind, 0, "typecase_tail")
        .unwrap();
    let Some(BasicTypeEnum::IntType(tag)) = agg_type.get_field_type_at_index(2) else {
        panic!()
    };
    let tag = tag.const_int(case_index as u64, false);
    let agg = builder
        .build_insert_value(agg, tag, 2, "typecase_tag")
        .unwrap();
    // as llvm is a cunt, do some pointer fuckery instead
    // let agg = builder
    //     .build_bit_cast(agg, ret, "typecase_to_opaque")
    //     .unwrap();
    let agg = {
        let agg_ptr = builder.build_alloca(agg_type, "alloca_typecase").unwrap();
        builder.build_store(agg_ptr, agg).unwrap();
        builder.build_load(ret, agg_ptr, "*(type*)typecase").unwrap()
    };
    builder.build_return(Some(&agg)).unwrap();

    Out::Func(val)
}

fn functions(f: FunctionId, tara: &mut Tara) -> Out {
    let ty = tara.quir_items[f].typ.kind;
    let ty = tara.quir_types[ty].cheap();
    let llvm_ctx = tara.llvm_mod.get_context();
    let AnyTypeEnum::FunctionType(ty) = lower_type(ty, None, tara, llvm_ctx) else {
        panic!("non-function typed functions??")
    };
    let val = tara
        .llvm_mod
        .add_function(tara.quir_items[f].name.name.0, ty, None);
    let entry = llvm_ctx.append_basic_block(val, "entry");
    let ret_b = llvm_ctx.append_basic_block(val, "return");
    let builder = llvm_ctx.create_builder();
    builder.position_at_end(entry);
    let ret_v = builder
        .build_alloca(ty.get_return_type().unwrap(), "&return_slot")
        .unwrap();
    let mut ctx = Ctx {
        lets: Default::default(),
        loops: Vec::new(),
        ctx: llvm_ctx,
        id: f,
        builder,
        ret_v,
        ret_b,
        tara,
        val,
    };
    let body = ctx.tara.quir_items[ctx.id].body;
    match body.codegen(&mut ctx) {
        Err(Control::Return) => {}
        Err(Control::Break) => panic!("breaks in plain bareblock shouldn't happen"),
        Ok(body) => {
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
    Out::Func(val)
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
    t: Typekind,
    args: Option<Typekind>,
    tara: &mut Tara,
    ctx: ContextRef<'static>,
) -> AnyTypeEnum<'static> {
    match t {
        Typekind::Bundle(_) => panic!("found bundle outside of call!"),
        Typekind::Var(n) => panic!("well something went wrong!\nfound type var: {}", n),
        Typekind::Func { args, ret } => {
            let args = tara.quir_types[args].clone();
            let args: BasicMetadataTypeEnum = match lower_type(args, None, tara, ctx) {
                AnyTypeEnum::FunctionType(_) => ctx.ptr_type(AddressSpace::from(0u16)).into(),
                AnyTypeEnum::ArrayType(t) => t.into(),
                AnyTypeEnum::FloatType(t) => t.into(),
                AnyTypeEnum::IntType(t) => t.into(),
                AnyTypeEnum::PointerType(t) => t.into(),
                AnyTypeEnum::StructType(t) => t.into(),
                AnyTypeEnum::VectorType(t) => t.into(),
                AnyTypeEnum::VoidType(_) => panic!("I have no idea"),
            };
            let ret = tara.quir_types[ret].clone();
            let ret = any_to_basic_type(lower_type(ret, None, tara, ctx), ctx);
            ret.fn_type(&[args], false).into()
        }
        Typekind::Call { args, func } => {
            let args = tara.quir_types[args].clone();
            let func = tara.quir_types[func].clone();
            lower_type(func, Some(args), tara, ctx)
        }
        Typekind::String => ctx.ptr_type(AddressSpace::from(0u16)).into(),
        Typekind::Bool => ctx.custom_width_int_type(1).into(),
        Typekind::Int => ctx.i64_type().into(),
        Typekind::Tup => {
            let Some(Typekind::Bundle(args)) = args else {
                panic!("uninstantiated tuples??")
            };
            let mut fields = Vec::with_capacity(args.len());
            for &a in args.iter() {
                let a = tara.quir_types[a].clone();
                let a: BasicTypeEnum = match lower_type(a, None, tara, ctx) {
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
        Typekind::Recall(_, _, None) => panic!("resolve'da"),
        Typekind::Recall(_, _, Some(id)) => match tara.codegen(In { i: id }) {
            Out::Func(_) | Out::Namespace(_) => panic!("why is a non-type id in the type?"),
            Out::Typedecl(t, _) => t.into(),
        },
    }
}

pub struct Ctx<'a> {
    builder: Builder<'static>,
    ctx: ContextRef<'static>,
    val: FunctionValue<'static>,
    lets: Map<Istr, (BasicTypeEnum<'static>, PointerValue<'static>)>,
    id: FunctionId,
    ret_v: PointerValue<'static>,
    ret_b: BasicBlock<'static>,
    loops: Vec<(BasicBlock<'static>, PointerValue<'static>)>,
    tara: &'a mut Tara,
}
impl std::ops::Index<ExprId> for Ctx<'_> {
    type Output = Expr;
    fn index(&self, id: ExprId) -> &Self::Output {
        &self.tara.quir_items[self.id].locals[id]
    }
}
impl std::ops::Index<BindingId> for Ctx<'_> {
    type Output = Binding;
    fn index(&self, id: BindingId) -> &Self::Output {
        &self.tara.quir_items[self.id].locals[id]
    }
}

impl ExprId {
    fn codegen(&self, ctx: &mut Ctx) -> Result {
        let this = ctx[*self].cheap();
        this.kind.codegen(this.typ, ctx)
    }
}

impl expr::If {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let cond = match self.cond.codegen(ctx)? {
            AnyValueEnum::IntValue(c) => c,
            _ => panic!("typechecker should've caught this"),
        };
        let smash_b = ctx.ctx.append_basic_block(ctx.val, "smash");
        let pass_b = ctx.ctx.append_basic_block(ctx.val, "pass");
        let post_b = ctx.ctx.append_basic_block(ctx.val, "post");
        ctx.builder
            .build_conditional_branch(cond, smash_b, pass_b)
            .unwrap();
        let mut cont = false;

        ctx.builder.position_at_end(smash_b);
        let smash_v = self.smash.codegen(ctx);
        if smash_v.is_ok() {
            ctx.builder.build_unconditional_branch(post_b).unwrap();
            cont = true;
        }
        ctx.builder.position_at_end(pass_b);
        let pass_v = self.pass.codegen(ctx);
        if pass_v.is_ok() {
            ctx.builder.build_unconditional_branch(post_b).unwrap();
            cont = true;
        }

        ctx.builder.position_at_end(post_b);
        if !cont {
            ctx.builder.build_unreachable().unwrap();
            if Err(Control::Break) == smash_v {
                return smash_v;
            }
            if let Err(Control::Break) = pass_v {
                return pass_v;
            }
            return smash_v;
        }

        let (l, p, q) = match (smash_v, pass_v) {
            (Ok(p), Ok(q)) => (
                2,
                (any_to_basic_value(p), smash_b),
                (any_to_basic_value(q), pass_b),
            ),
            (Ok(p), _) => (
                1,
                (any_to_basic_value(p), smash_b),
                (ctx.ctx.const_struct(&[], false).into(), pass_b),
            ),
            (_, Ok(p)) => (
                1,
                (any_to_basic_value(p), pass_b),
                (ctx.ctx.const_struct(&[], false).into(), smash_b),
            ),
            _ => unreachable!(),
        };
        let incoming: &[(&dyn BasicValue, _)] = if l == 1 {
            &[(&p.0, p.1)]
        } else {
            &[(&p.0, p.1), (&q.0, q.1)]
        };

        let phi_v = ctx
            .builder
            .build_phi(p.0.get_type(), "merge_branches")
            .unwrap();
        phi_v.add_incoming(incoming);
        Ok(phi_v.into())
    }
}

impl expr::Call {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        if let expr::Kind::Builtin(b) = ctx[self.func].kind {
            let typ = ctx[self.func].typ;
            b.codegen_real(self.args, typ, ctx)
        } else {
            let func_v = match self.func.codegen(ctx)? {
                AnyValueEnum::FunctionValue(v) => v,
                _ => panic!("help me"),
            };
            let args = self.args.codegen(ctx)?;
            let res = ctx
                .builder
                .build_call(func_v, &[any_to_basic_value(args).into()], "call")
                .unwrap();
            Ok(res.as_any_value_enum())
        }
    }
}

impl expr::Builtinkind {
    pub fn codegen(&self, _: Type, _: &mut Ctx) -> Result {
        panic!("builtin shouldn't call codegen as it requires arguments!")
    }

    pub fn codegen_real(&self, args_id: ExprId, _: Type, ctx: &mut Ctx) -> Result {
        let mut args = Vec::with_capacity(7);
        match ctx[args_id].kind {
            expr::Kind::Tuple(ref t) => {
                for &f in t.0.cheap().iter() {
                    let f = f.codegen(ctx)?;
                    args.push(f);
                }
            }
            _ => {
                let a = args_id.codegen(ctx)?;
                args.push(a);
            }
        }
        let name = self.spelling();
        use expr::Builtinkind::*;
        Ok(match self {
            Add => ctx
                .builder
                .build_int_add(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            Sub => ctx
                .builder
                .build_int_sub(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            Mul => ctx
                .builder
                .build_int_mul(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            Div => ctx
                .builder
                .build_int_unsigned_div(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            Mod => ctx
                .builder
                .build_int_unsigned_rem(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            And => ctx
                .builder
                .build_and(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            Or => ctx
                .builder
                .build_or(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            Xor => ctx
                .builder
                .build_xor(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            ShLeft => ctx
                .builder
                .build_left_shift(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            ShRight => ctx
                .builder
                .build_right_shift(
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    false,
                    name,
                )
                .unwrap()
                .into(),
            Not => ctx
                .builder
                .build_not(args[0].into_int_value(), name)
                .unwrap()
                .into(),
            Negate => ctx
                .builder
                .build_int_neg(args[0].into_int_value(), name)
                .unwrap()
                .into(),
            CmpEq => ctx
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            CmpNE => ctx
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            CmpGt => ctx
                .builder
                .build_int_compare(
                    IntPredicate::UGT,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            CmpLt => ctx
                .builder
                .build_int_compare(
                    IntPredicate::ULT,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            CmpGE => ctx
                .builder
                .build_int_compare(
                    IntPredicate::UGE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            CmpLE => ctx
                .builder
                .build_int_compare(
                    IntPredicate::ULE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            IntToPtr => ctx
                .builder
                .build_int_to_ptr(
                    args[0].into_int_value(),
                    ctx.ctx.ptr_type(AddressSpace::from(0)),
                    name,
                )
                .unwrap()
                .into(),
            PtrToInt => ctx
                .builder
                .build_ptr_to_int(args[0].into_pointer_value(), ctx.ctx.i64_type(), name)
                .unwrap()
                .into(),
            Syscall => {
                let asm_t = ctx
                    .ctx
                    .i64_type()
                    .fn_type(&[ctx.ctx.i64_type().into(); 7], false);
                let asm_v = ctx.ctx.create_inline_asm(
                    asm_t,
                    "syscall".into(),
                    "=r,{rax},{rdi},{rsi},{rdx},{r8},{r9},{r10}".into(),
                    true,
                    false,
                    Some(InlineAsmDialect::Intel),
                    false,
                );
                ctx.builder
                    .build_indirect_call(
                        asm_t,
                        asm_v,
                        &[
                            args[0].into_int_value().into(),
                            args[1].into_int_value().into(),
                            args[2].into_int_value().into(),
                            args[3].into_int_value().into(),
                            args[4].into_int_value().into(),
                            args[5].into_int_value().into(),
                            args[6].into_int_value().into(),
                        ],
                        name,
                    )
                    .unwrap()
                    .as_any_value_enum()
            }
        })
    }
}

impl expr::Tuple {
    pub fn codegen(&self, typ: Type, ctx: &mut Ctx) -> Result {
        let expr_ids = self.0.cheap();
        let ty = ctx.tara.quir_types[typ.kind].cheap();
        let AnyTypeEnum::StructType(ty) = lower_type(ty, None, ctx.tara, ctx.ctx) else {
            panic!("aaaaaaaa")
        };
        let mut comma = ty.get_undef().into();
        for (ix, &id) in expr_ids.iter().enumerate() {
            let v = any_to_basic_value(id.codegen(ctx)?);
            comma = ctx
                .builder
                .build_insert_value(comma, v, ix as u32, "tuple")
                .unwrap();
        }
        Ok(comma.as_any_value_enum())
    }
}

impl expr::Loop {
    pub fn codegen(&self, typ: Type, ctx: &mut Ctx) -> Result {
        let ty = typ.kind;
        let ty = ctx.tara.quir_types[ty].clone();
        let ty = lower_type(ty, None, ctx.tara, ctx.ctx);
        let ty = any_to_basic_type(ty, ctx.ctx);
        let post_v = ctx.builder.build_alloca(ty, "&loop_slot").unwrap();
        let loop_b = ctx.ctx.append_basic_block(ctx.val, "loop");
        let post_b = ctx.ctx.append_basic_block(ctx.val, "loop_end");
        ctx.loops.push((post_b, post_v));
        ctx.builder.build_unconditional_branch(loop_b).unwrap();
        ctx.builder.position_at_end(loop_b);
        let v = self.0.codegen(ctx);
        if Err(Control::Return) == v {
            return v;
        }
        if v.is_ok() {
            ctx.builder.build_unconditional_branch(loop_b).unwrap();
        }
        ctx.builder.position_at_end(post_b);
        ctx.loops.pop();
        let v = ctx.builder.build_load(ty, post_v, "*loop_slot").unwrap();
        Ok(v.into())
    }
}

impl expr::Bareblock {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let mut out = ctx.ctx.const_struct(&[], false).into();
        for &id in self.0.cheap().iter() {
            out = id.codegen(ctx)?;
        }
        Ok(out)
    }
}

impl expr::Recall {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        match self.1.expect("resolve'da") {
            // name resolution done before the typer is useless now
            // as the data we need is not a field in the struct
            Left(l) => {
                let binding::Kind::Name(n) = ctx[l].kind else {
                    panic!("I have no idea")
                };
                let (ty, ptr) = ctx.lets.get(&n.0).copied().unwrap();
                let out = ctx.builder.build_load(ty, ptr, "recall_local").unwrap();
                Ok(out.into())
            }
            // 'cycles' like /this/ are fine, the value exists already
            Right(g) => Ok(if g == ctx.id.into() {
                ctx.val.into()
            } else {
                let Out::Func(f) = ctx.tara.codegen(In { i: g }) else {
                    panic!("huh?");
                };
                f.into()
            }),
        }
    }
}

impl expr::Number {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let v = ctx
            .ctx
            .custom_width_int_type(64)
            .const_int_from_string(self.0 .0, StringRadix::Decimal)
            .unwrap();
        Ok(v.into())
    }
}

impl expr::String {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let global = ctx
            .builder
            .build_global_string_ptr(self.0 .0, "string_literal")
            .unwrap();
        Ok(global.as_pointer_value().into())
    }
}

impl expr::Bool {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let v = if self.0 == "true".into() { 1 } else { 0 };
        let v = ctx.ctx.custom_width_int_type(1).const_int(v, false);
        Ok(v.into())
    }
}

impl expr::Arguments {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        Ok(ctx.val.get_nth_param(0).unwrap().into())
    }
}

impl expr::Poison {
    pub fn codegen(&self, _: Type, _: &mut Ctx) -> Result {
        panic!("Poison should've barred codegen from running earlier on!")
    }
}

impl expr::Let {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let v = self.1.codegen(ctx)?;
        let v = any_to_basic_value(v);
        self.0.codegen(v, ctx);
        Ok(ctx.ctx.const_struct(&[], false).into())
    }
}

impl expr::Assign {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let v = self.2.codegen(ctx)?;
        let (_, ptr) = ctx.lets.get(&self.0.name).unwrap();
        ctx.builder
            .build_store(*ptr, any_to_basic_value(v))
            .unwrap();
        Ok(ctx.ctx.const_struct(&[], false).into())
    }
}

impl expr::Break {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let v = self.val.codegen(ctx)?;
        let (block, ptr) = ctx.loops.last().unwrap();
        ctx.builder
            .build_store(*ptr, any_to_basic_value(v))
            .unwrap();
        ctx.builder.build_unconditional_branch(*block).unwrap();
        Err(Control::Break)
    }
}

impl expr::Return {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        let v = self.0.codegen(ctx)?;
        ctx.builder
            .build_store(ctx.ret_v, any_to_basic_value(v))
            .unwrap();
        ctx.builder.build_unconditional_branch(ctx.ret_b).unwrap();
        Err(Control::Return)
    }
}

impl expr::Const {
    pub fn codegen(&self, _: Type, ctx: &mut Ctx) -> Result {
        self.0.codegen(ctx)?;
        Ok(ctx.ctx.const_struct(&[], false).into())
    }
}

impl BindingId {
    fn codegen(&self, v: BasicValueEnum<'static>, ctx: &mut Ctx) {
        ctx[*self].kind.cheap().codegen(v, ctx);
    }
}

impl binding::Empty {
    pub fn codegen(&self, _: BasicValueEnum, _: &mut Ctx) {}
}

impl binding::Name {
    pub fn codegen(&self, v: BasicValueEnum<'static>, ctx: &mut Ctx) {
        let ty = v.get_type();
        let ptr = ctx.builder.build_alloca(ty, "let").unwrap();
        ctx.builder.build_store(ptr, v).unwrap();
        ctx.lets.insert(self.0, (ty, ptr));
    }
}

impl binding::Tuple {
    pub fn codegen(&self, v: BasicValueEnum<'static>, ctx: &mut Ctx) {
        let BasicValueEnum::StructValue(v) = v else {
            panic!("typer shouldn't allow anything else")
        };
        for (i, &b) in self.0.cheap().iter().enumerate() {
            let v = ctx
                .builder
                .build_extract_value(v, i as u32, "destructure")
                .unwrap();
            b.codegen(v, ctx);
        }
    }
}
