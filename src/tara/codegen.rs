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
    fill,
    misc::{CheapClone, Istr},
};

use super::{
    uir::{self, ExprId, FuncId, TypecaseId, TypedeclId},
    Tara,
};

#[derive(Clone)]
pub enum Out {
    Typedecl(StructType<'static>, Rc<[StructType<'static>]>),
    Func(FunctionValue<'static>),
    Namespace(Rc<[Out]>),
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
    Break,
    Return,
}

type Result = std::result::Result<AnyValueEnum<'static>, Control>;

fn codegen(tara: &mut Tara, i: In) -> Out {
    tara.fill(fill::In { i: i.i });
    match tara.uir_items[i.i] {
        uir::Item::Typecase(_) => typecases(i.i.into_typecase(), tara),
        uir::Item::Typedecl(_) => typedecls(i.i.into_typedecl(), tara),
        uir::Item::Function(_) => functions(i.i.into_func(), tara),
        uir::Item::Namespace(_) => {
            let is = tara.uir_interfaces[i.i].into_namespace().items.clone();
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
    let cases = tara.uir_items[d].cases.cheap();
    let tag_size = cases.len().next_power_of_two().ilog2();
    let tag_type = ctx.custom_width_int_type(tag_size);
    let tag_align = tara.target.get_target_data().get_abi_alignment(&tag_type);
    let cases_tails: Vec<_> = cases
        .iter()
        .map(|&c| {
            let bind = tara.uir_interfaces[c].binding;
            let bind = tara[(c, bind)].typ.kind;
            let bind = tara.uir_types[bind].cheap();
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
    let case = tara.uir_interfaces[c];
    let name = case.name.name.0;
    let ty = tara.uir_types[case.typ.kind].clone();
    let llvm_ctx = tara.llvm_mod.get_context();
    let AnyTypeEnum::FunctionType(ty) = lower_type(ty, None, tara, llvm_ctx) else {
        panic!("constructors should be functions?")
    };
    let (ret, agg_type) = match tara.codegen(In {
        i: case.parent.into(),
    }) {
        Out::Typedecl(ret, cases_type) => (ret, cases_type[case.index]),
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
    let tag = tag.const_int(case.index as u64, false);
    let agg = builder
        .build_insert_value(agg, tag, 2, "typecase_tag")
        .unwrap();
    let agg = builder.build_bit_cast(agg, ret, "typecase_to_opaque").unwrap();
    builder.build_return(Some(&agg)).unwrap();

    Out::Func(val)
}

fn functions(f: FuncId, tara: &mut Tara) -> Out {
    let ty = tara.item_type(f.into(), None).kind;
    let ty = tara.uir_types[ty].clone();
    let llvm_ctx = tara.llvm_mod.get_context();
    let AnyTypeEnum::FunctionType(ty) = lower_type(ty, None, tara, llvm_ctx) else {
        panic!("non-function typed functions??")
    };
    let val = tara
        .llvm_mod
        .add_function(tara.item_name(f).name.0, ty, None);
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
        id: f,
        builder,
        ret_v,
        ret_b,
        tara,
        val,
    };
    match ctx.expressions(ctx.tara.uir_items[ctx.id].body) {
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
    t: uir::Typekind,
    args: Option<uir::Typekind>,
    tara: &mut Tara,
    ctx: ContextRef<'static>,
) -> AnyTypeEnum<'static> {
    match t {
        uir::Typekind::Bundle(_) => panic!("found bundle outside of call!"),
        uir::Typekind::Var(n) => panic!("well something went wrong!\nfound type var: {}", n),
        uir::Typekind::Func { args, ret } => {
            let args = tara.uir_types[args].clone();
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
            let ret = tara.uir_types[ret].clone();
            let ret = any_to_basic_type(lower_type(ret, None, tara, ctx), ctx);
            ret.fn_type(&[args], false).into()
        }
        uir::Typekind::Call { args, func } => {
            let args = tara.uir_types[args].clone();
            let func = tara.uir_types[func].clone();
            lower_type(func, Some(args), tara, ctx)
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
                let a = tara.uir_types[a].clone();
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
        uir::Typekind::Recall(id) => match tara.codegen(In { i: id }) {
            Out::Func(_) | Out::Namespace(_) => panic!("why is a non-type id in the type?"),
            Out::Typedecl(t, _) => t.into(),
        },
    }
}

struct Context<'a> {
    builder: Builder<'static>,
    ctx: ContextRef<'static>,
    val: FunctionValue<'static>,
    lets: Map<Istr, (BasicTypeEnum<'static>, PointerValue<'static>)>,
    id: FuncId,
    ret_v: PointerValue<'static>,
    ret_b: BasicBlock<'static>,
    loops: Vec<(BasicBlock<'static>, PointerValue<'static>)>,
    tara: &'a mut Tara,
}

impl Context<'_> {
    fn expressions(&mut self, e: ExprId) -> Result {
        match self.tara[(self.id, e)].kind {
            uir::Exprkind::If { cond, smash, pass } => {
                let cond = match self.expressions(cond)? {
                    AnyValueEnum::IntValue(c) => c,
                    _ => panic!("typechecker should've caught this"),
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
                if smash_v.is_ok() {
                    self.builder.build_unconditional_branch(post_b).unwrap();
                    cont = true;
                }
                self.builder.position_at_end(pass_b);
                let pass_v = self.expressions(pass);
                if pass_v.is_ok() {
                    self.builder.build_unconditional_branch(post_b).unwrap();
                    cont = true;
                }

                self.builder.position_at_end(post_b);
                if !cont {
                    self.builder.build_unreachable().unwrap();
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
                        (self.ctx.const_struct(&[], false).into(), pass_b),
                    ),
                    (_, Ok(p)) => (
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
                Ok(phi_v.into())
            }
            uir::Exprkind::Call { func, args } => {
                if let uir::Exprkind::Builtin(b) = self.tara[(self.id, func)].kind {
                    self.builtins(b, args)
                } else {
                    let func_v = match self.expressions(func)? {
                        AnyValueEnum::FunctionValue(v) => v,
                        _ => panic!("help me"),
                    };
                    let args = self.expressions(args)?;
                    let res = self
                        .builder
                        .build_call(func_v, &[any_to_basic_value(args).into()], "call")
                        .unwrap();
                    Ok(res.as_any_value_enum())
                }
            }
            uir::Exprkind::Builtin(_) => {
                panic!("builtins shouldn't be used for anything other than calls")
            }
            uir::Exprkind::Tuple(ref expr_ids) => {
                let expr_ids = expr_ids.clone();
                let ty = self.tara.uir_types[self.tara[(self.id, e)].typ.kind].clone();
                let AnyTypeEnum::StructType(ty) = lower_type(ty, None, self.tara, self.ctx)
                else {
                    panic!("aaaaaaaa")
                };
                let mut comma = ty.get_undef().into();
                for (ix, &id) in expr_ids.iter().enumerate() {
                    let v = any_to_basic_value(self.expressions(id)?);
                    comma = self
                        .builder
                        .build_insert_value(comma, v, ix as u32, "tuple")
                        .unwrap();
                }
                Ok(comma.as_any_value_enum())
            }
            uir::Exprkind::Loop(expr_id) => {
                let ty = self.tara[(self.id, e)].typ.kind;
                let ty = self.tara.uir_types[ty].clone();
                let ty = lower_type(ty, None, self.tara, self.ctx);
                let ty = any_to_basic_type(ty, self.ctx);
                let post_v = self.builder.build_alloca(ty, "&loop_slot").unwrap();
                let loop_b = self.ctx.append_basic_block(self.val, "loop");
                let post_b = self.ctx.append_basic_block(self.val, "loop_end");
                self.loops.push((post_b, post_v));
                self.builder.build_unconditional_branch(loop_b).unwrap();
                self.builder.position_at_end(loop_b);
                let v = self.expressions(expr_id);
                if Err(Control::Return) == v {
                    return v;
                }
                if v.is_ok() {
                    self.builder.build_unconditional_branch(loop_b).unwrap();
                }
                self.builder.position_at_end(post_b);
                self.loops.pop();
                let v = self.builder.build_load(ty, post_v, "*loop_slot").unwrap();
                Ok(v.into())
            }
            uir::Exprkind::Bareblock(ref expr_ids) => {
                let mut out = self.ctx.const_struct(&[], false).into();
                let expr_ids = expr_ids.clone();
                for &id in expr_ids.iter() {
                    out = self.expressions(id)?;
                }
                Ok(out)
            }
            // name resolution done before the typer is useless now
            // as the data we need is not a field in the struct
            uir::Exprkind::Recall(Ok(l)) => {
                let uir::Bindkind::Name(n, _) = self.tara[(self.id, l)].kind else {
                    panic!("I have no idea")
                };
                let (ty, ptr) = self.lets.get(&n).copied().unwrap();
                let out = self.builder.build_load(ty, ptr, "recall_local").unwrap();
                Ok(out.into())
            }
            // 'cycles' like /this/ are fine, the value exists already
            uir::Exprkind::Recall(Err(g)) => Ok(if g == self.id.into() {
                self.val.into()
            } else {
                let Out::Func(f) = self.tara.codegen(In { i: g }) else {
                    panic!("huh?");
                };
                f.into()
            }),

            uir::Exprkind::Number(istr) => {
                let v = self
                    .ctx
                    .custom_width_int_type(64)
                    .const_int_from_string(istr.0, StringRadix::Decimal)
                    .unwrap();
                Ok(v.into())
            }
            uir::Exprkind::String(istr) => {
                let global = self
                    .builder
                    .build_global_string_ptr(istr.0, "string_literal")
                    .unwrap();
                Ok(global.as_pointer_value().into())
            }
            uir::Exprkind::Bool(istr) => {
                let v = if istr == "true".into() { 1 } else { 0 };
                let v = self.ctx.custom_width_int_type(1).const_int(v, false);
                Ok(v.into())
            }
            uir::Exprkind::Arguments => Ok(self.val.get_nth_param(0).unwrap().into()),
            uir::Exprkind::Poison => {
                panic!("Poison should've barred codegen from running earlier on!")
            }
            uir::Exprkind::Let(binding_id, expr_id) => {
                let v = self.expressions(expr_id)?;
                let v = any_to_basic_value(v);
                self.bindings(binding_id, v);
                Ok(self.ctx.const_struct(&[], false).into())
            }
            uir::Exprkind::Assign(l, expr_id) => {
                let uir::Bindkind::Name(n, _) = self.tara[(self.id, l)].kind else {
                    panic!("I have no idea")
                };
                let v = self.expressions(expr_id)?;
                let (_, ptr) = self.lets.get(&n).unwrap();
                self.builder
                    .build_store(*ptr, any_to_basic_value(v))
                    .unwrap();
                Ok(self.ctx.const_struct(&[], false).into())
            }
            uir::Exprkind::Break { val, target: _ } => {
                let v = self.expressions(val)?;
                let (block, ptr) = self.loops.last().unwrap();
                self.builder
                    .build_store(*ptr, any_to_basic_value(v))
                    .unwrap();
                self.builder.build_unconditional_branch(*block).unwrap();
                Err(Control::Break)
            }
            uir::Exprkind::Return(expr_id) => {
                let v = self.expressions(expr_id)?;
                self.builder
                    .build_store(self.ret_v, any_to_basic_value(v))
                    .unwrap();
                self.builder.build_unconditional_branch(self.ret_b).unwrap();
                Err(Control::Return)
            }
            uir::Exprkind::Const(expr_id) => {
                self.expressions(expr_id)?;
                Ok(self.ctx.const_struct(&[], false).into())
            }
        }
    }

    fn bindings(&mut self, b: uir::BindingId, v: BasicValueEnum<'static>) {
        match self.tara[(self.id, b)].kind {
            uir::Bindkind::Empty => {}
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

    fn builtins(&mut self, b: uir::Builtinkind, a: uir::ExprId) -> Result {
        let mut args = Vec::with_capacity(7);
        match self.tara[(self.id, a)].kind {
            uir::Exprkind::Tuple(ref fields) => {
                for &f in fields.clone().iter() {
                    let f = self.expressions(f)?;
                    args.push(f);
                }
            }
            _ => {
                let a = self.expressions(a)?;
                args.push(a);
            }
        }

        let name = b.spelling();
        Ok(match b {
            uir::Builtinkind::Add => self
                .builder
                .build_int_add(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Sub => self
                .builder
                .build_int_sub(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Mul => self
                .builder
                .build_int_mul(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Div => self
                .builder
                .build_int_unsigned_div(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Mod => self
                .builder
                .build_int_unsigned_rem(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::And => self
                .builder
                .build_and(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Or => self
                .builder
                .build_or(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Xor => self
                .builder
                .build_xor(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::ShLeft => self
                .builder
                .build_left_shift(args[0].into_int_value(), args[1].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::ShRight => self
                .builder
                .build_right_shift(
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    false,
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::Not => self
                .builder
                .build_not(args[0].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Negate => self
                .builder
                .build_int_neg(args[0].into_int_value(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::CmpEq => self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::CmpNE => self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::CmpGt => self
                .builder
                .build_int_compare(
                    IntPredicate::UGT,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::CmpLt => self
                .builder
                .build_int_compare(
                    IntPredicate::ULT,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::CmpGE => self
                .builder
                .build_int_compare(
                    IntPredicate::UGE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::CmpLE => self
                .builder
                .build_int_compare(
                    IntPredicate::ULE,
                    args[0].into_int_value(),
                    args[1].into_int_value(),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::IntToPtr => self
                .builder
                .build_int_to_ptr(
                    args[0].into_int_value(),
                    self.ctx.ptr_type(AddressSpace::from(0)),
                    name,
                )
                .unwrap()
                .into(),
            uir::Builtinkind::PtrToInt => self
                .builder
                .build_ptr_to_int(args[0].into_pointer_value(), self.ctx.i64_type(), name)
                .unwrap()
                .into(),
            uir::Builtinkind::Syscall => {
                let asm_t = self
                    .ctx
                    .i64_type()
                    .fn_type(&[self.ctx.i64_type().into(); 7], false);
                let asm_v = self.ctx.create_inline_asm(
                    asm_t,
                    "syscall".into(),
                    "=r,{rax},{rdi},{rsi},{rdx},{r8},{r9},{r10}".into(),
                    true,
                    false,
                    Some(InlineAsmDialect::Intel),
                    false,
                );
                self.builder
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
