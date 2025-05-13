use crate::{
    control::{Qmap, Query},
    data::{
        files::Files,
        quir::{self, binding, expr, types},
        Codegen, Quir,
    },
    fill, resolve, typer,
};
use either::Either::{Left, Right};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::ContextRef,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, StringRadix, StructType},
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, InlineAsmDialect, IntPredicate,
};
use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub enum Data {
    Typedecl(Typedecl),
    Func(FunctionValue<'static>),
}
#[derive(Default, Debug)]
pub struct Submaps {
    maptd: Maptd,
    maptc: Maptc,
    mapty: Mapty,
    mapfn: Mapfn,
}
pub type Map = Qmap<quir::Id, Data, Data>;
impl Query<quir::Id, Data> for Data {
    type Inputs<'a> = (
        &'a Files,
        &'a mut Quir,
        &'a mut Submaps,
        &'a mut typer::Map,
        &'a mut resolve::Map,
        &'a mut fill::Map,
        &'a mut Codegen,
    );

    fn query(
        _: &mut Map,
        &id: &quir::Id,
        (
            files,
            quir,
            Submaps {
                maptd,
                maptc,
                mapty,
                mapfn,
            },
            tymap,
            resmap,
            fillmap,
            cg,
        ): Self::Inputs<'_>,
    ) -> Data {
        fillmap.query(id, (files, tymap, &quir.items, &mut quir.types, resmap));
        match id {
            quir::Id::Typecase(id) => Data::Func(maptc.query(id, (cg, quir, mapty, maptd))),
            quir::Id::Typedecl(id) => Data::Typedecl(maptd.query(id, (cg, quir, mapty))),
            quir::Id::Function(id) => Data::Func(mapfn.query(id, (cg, quir, maptd, maptc, mapty))),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Typedecl(StructType<'static>, Rc<[StructType<'static>]>);
type Maptd = Qmap<quir::TypedeclId, Typedecl, ()>;
impl Query<quir::TypedeclId, Typedecl> for () {
    type Inputs<'a> = (&'a mut Codegen, &'a Quir, &'a mut Mapty);

    fn query(
        map: &mut Maptd,
        &id: &quir::TypedeclId,
        (cg, quir, cgtymap): Self::Inputs<'_>,
    ) -> Typedecl {
        let lcx = cg.module.get_context();
        let cases = &quir.items.typedecls[id].cases;
        let tag_size = cases.len().next_power_of_two().ilog2();
        let tag_type = lcx.custom_width_int_type(tag_size);
        let tag_align = cg.target.get_target_data().get_abi_alignment(&tag_type);

        let cases_tails: Vec<_> = cases
            .iter()
            .map(|&c| {
                let bind = quir.items[c].binding;
                let bind = quir.items[c].bindings[bind].typ.kind;
                let bind = cgtymap.query((bind, None), (cg, quir, map));
                cg.anytype_to_basic(bind)
            })
            .collect();
        let tail_size = cases_tails
            .iter()
            .map(|t| cg.target.get_target_data().get_abi_size(t))
            .max()
            .unwrap_or(0)
            .next_multiple_of(tag_align as u64);

        let opaque_type = lcx.struct_type(
            &[
                lcx.custom_width_int_type(tail_size as u32 * 8).into(),
                tag_type.into(),
            ],
            false,
        );
        let tail_type: Vec<_> = cases_tails
            .into_iter()
            .map(|tail| {
                let padding = tail_size - cg.target.get_target_data().get_abi_size(&tail);
                lcx.struct_type(
                    &[
                        tail,
                        lcx.custom_width_int_type(padding as u32 * 8).into(),
                        tag_type.into(),
                    ],
                    false,
                )
            })
            .collect();

        Typedecl(opaque_type, tail_type.into())
    }
}

type Maptc = Qmap<quir::TypecaseId, FunctionValue<'static>, ()>;
impl Query<quir::TypecaseId, FunctionValue<'static>> for () {
    type Inputs<'a> = (&'a mut Codegen, &'a Quir, &'a mut Mapty, &'a mut Maptd);

    fn query(
        _: &mut Maptc,
        &id: &quir::TypecaseId,
        (cg, quir, mapty, maptd): Self::Inputs<'_>,
    ) -> FunctionValue<'static> {
        let quir::Typecase {
            binding,
            parent,
            index,
            name,
            ..
        } = quir.items.typecases[id];
        let binding_ty = quir.items.typecases[id].bindings[binding].typ.kind;
        let binding_ty = mapty.query((binding_ty, None), (cg, quir, maptd));
        let binding_ty = cg.anytype_to_basic(binding_ty);
        let Typedecl(ret_ty, agg_ty) = maptd.query(parent, (cg, quir, mapty));
        let agg_ty = agg_ty[index];
        let fn_ty = ret_ty.fn_type(&[binding_ty.into()], false);

        let lcx = cg.module.get_context();
        let val = cg.module.add_function(name.name, fn_ty, None);
        let entry = lcx.append_basic_block(val, "entry");
        let builder = lcx.create_builder();
        builder.position_at_end(entry);

        let agg = agg_ty.get_undef();
        let bind = val.get_first_param().unwrap();
        let agg = builder
            .build_insert_value(agg, bind, 0, "typecase_tail")
            .unwrap();
        let Some(BasicTypeEnum::IntType(tag)) = agg_ty.get_field_type_at_index(2) else {
            panic!()
        };
        let tag = tag.const_int(index as u64, false);
        let agg = builder
            .build_insert_value(agg, tag, 2, "typecase_tag")
            .unwrap();
        let agg = {
            let agg_ptr = builder.build_alloca(agg_ty, "alloca_typecase").unwrap();
            builder.build_store(agg_ptr, agg).unwrap();
            builder
                .build_load(ret_ty, agg_ptr, "*(type*)typecase")
                .unwrap()
        };
        builder.build_return(Some(&agg)).unwrap();

        val
    }
}

type Mapty = Qmap<(types::Id, Option<types::Id>), AnyTypeEnum<'static>, ()>;
impl Query<(types::Id, Option<types::Id>), AnyTypeEnum<'static>> for () {
    type Inputs<'a> = (&'a mut Codegen, &'a Quir, &'a mut Maptd);

    fn query(
        map: &mut Mapty,
        &(id, args): &(types::Id, Option<types::Id>),
        (cg, quir, maptd): Self::Inputs<'_>,
    ) -> AnyTypeEnum<'static> {
        use quir::types::Kind::*;
        let lcx = cg.module.get_context();
        match quir.types[id] {
            Bundle(_) => panic!("bundle type unused!"),
            Var(_) => panic!("typevar in codegen!"),
            Recall(types::Recall(_, _, ref id)) => {
                let &id = id.get().unwrap();
                maptd.query(id, (cg, quir, map)).0.into()
            }
            Func(types::Func { args, ret }) => {
                let args = map.query((args, None), (cg, quir, maptd));
                let args = cg.anytype_to_basic(args);
                let ret = map.query((ret, None), (cg, quir, maptd));
                let ret = cg.anytype_to_basic(ret);
                ret.fn_type(&[args.into()], false).into()
            }
            Call(types::Call { args, func }) => map.query((func, Some(args)), (cg, quir, maptd)),
            Tup(_) => {
                let args = args.unwrap();
                let Bundle(types::Bundle(ref args)) = quir.types.types[args] else {
                    panic!()
                };
                let mut fields = Vec::with_capacity(args.len());
                for &a in args {
                    let a = map.query((a, None), (cg, quir, maptd));
                    let a = cg.anytype_to_basic(a);
                    fields.push(a);
                }
                lcx.struct_type(fields.as_slice(), false).into()
            }
            String(_) => lcx.ptr_type(AddressSpace::default()).into(),
            Bool(_) => lcx.custom_width_int_type(1).into(),
            Int(_) => lcx.i64_type().into(),
        }
    }
}

impl Codegen {
    fn anytype_to_basic(&mut self, t: AnyTypeEnum<'static>) -> BasicTypeEnum<'static> {
        match t {
            AnyTypeEnum::ArrayType(t) => t.into(),
            AnyTypeEnum::FloatType(t) => t.into(),
            AnyTypeEnum::FunctionType(_) => self
                .module
                .get_context()
                .ptr_type(AddressSpace::default())
                .into(),
            AnyTypeEnum::IntType(t) => t.into(),
            AnyTypeEnum::PointerType(t) => t.into(),
            AnyTypeEnum::StructType(t) => t.into(),
            AnyTypeEnum::VectorType(t) => t.into(),
            AnyTypeEnum::VoidType(_) => panic!("the what brownie"),
        }
    }
    fn anyval_to_basic(&mut self, v: AnyValueEnum<'static>) -> BasicValueEnum<'static> {
        match v {
            AnyValueEnum::FunctionValue(v) => v.as_global_value().as_pointer_value().into(),
            AnyValueEnum::PhiValue(v) => v.as_basic_value(),
            AnyValueEnum::ArrayValue(v) => v.into(),
            AnyValueEnum::IntValue(v) => v.into(),
            AnyValueEnum::FloatValue(v) => v.into(),
            AnyValueEnum::PointerValue(v) => v.into(),
            AnyValueEnum::StructValue(v) => v.into(),
            AnyValueEnum::VectorValue(v) => v.into(),
            AnyValueEnum::InstructionValue(_) => panic!("who the"),
            AnyValueEnum::MetadataValue(_) => panic!("what"),
        }
    }
}

pub type Mapfn = Qmap<quir::FunctionId, FunctionValue<'static>, ()>;
impl Query<quir::FunctionId, FunctionValue<'static>> for () {
    type Inputs<'a> = (
        &'a mut Codegen,
        &'a Quir,
        &'a mut Maptd,
        &'a mut Maptc,
        &'a mut Mapty,
    );

    fn query(
        mapfn: &mut Mapfn,
        &id: &quir::FunctionId,
        (cg, quir, maptd, maptc, mapty): Self::Inputs<'_>,
    ) -> FunctionValue<'static> {
        let mut mapxp = Mapxp::default();
        let mut mapbd = Mapbd::default();

        let binding = quir.items[id].binding.kind;
        let binding = mapty.query((binding, None), (cg, quir, maptd));
        let binding = cg.anytype_to_basic(binding);
        let ret = quir.items[id].ret.kind;
        let ret = mapty.query((ret, None), (cg, quir, maptd));
        let ret = cg.anytype_to_basic(ret);
        let ty = ret.fn_type(&[binding.into()], false);
        let lcx = cg.module.get_context();
        let val = cg.module.add_function(quir.items[id].name.name, ty, None);
        let entry = lcx.append_basic_block(val, "entry");
        let ret_b = lcx.append_basic_block(val, "entry");
        let builder = lcx.create_builder();
        builder.position_at_end(entry);
        let ret_v = builder
            .build_alloca(ty.get_return_type().unwrap(), "&return_slot")
            .unwrap();
        let mut ctx = Ctx {
            lets: Default::default(),
            loops: Vec::new(),
            ifs: Vec::new(),
            builder,
            ret_v,
            ret_b,
            val,
            lcx,
            id,
        };
        let body = quir.items[id].body;
        match mapxp.query(
            body,
            (&mut ctx, cg, quir, mapty, maptd, maptc, mapfn, &mut mapbd),
        ) {
            Err(Control::Return) => {}
            Err(Control::Break) => panic!(),
            Ok((LocalValue::Value(body), _)) => {
                let body = cg.anyval_to_basic(body);
                ctx.builder.build_store(ctx.ret_v, body).unwrap();
                ctx.builder.build_unconditional_branch(ctx.ret_b).unwrap();
            }
            Ok((LocalValue::Place(ty, body), _)) => {
                let body = ctx.builder.build_load(ty, body, "deref_local").unwrap();
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
        val
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Control {
    Break,
    Return,
}
impl Control {
    fn min(self, other: Self) -> Self {
        match (self, other) {
            (Self::Break, _) | (_, Self::Break) => Self::Break,
            _ => Self::Return,
        }
    }
}
pub type Result = std::result::Result<(LocalValue, BasicBlock<'static>), Control>;

pub struct Ctx {
    builder: Builder<'static>,
    lcx: ContextRef<'static>,
    val: FunctionValue<'static>,
    lets: HashMap<&'static str, LocalValue>,
    id: quir::FunctionId,
    ret_v: PointerValue<'static>,
    ret_b: BasicBlock<'static>,
    ifs: Vec<BasicBlock<'static>>,
    loops: Vec<(BasicBlock<'static>, PointerValue<'static>)>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalValue {
    Value(AnyValueEnum<'static>),
    Place(BasicTypeEnum<'static>, PointerValue<'static>),
}
impl LocalValue {
    fn value(self, ctx: &mut Ctx) -> AnyValueEnum<'static> {
        match self {
            LocalValue::Value(v) => v,
            LocalValue::Place(t, p) => ctx.builder.build_load(t, p, "deref_place").unwrap().into(),
        }
    }
}

pub type Mapxp = Qmap<expr::Id, Result, expr::Id>;
pub type Inputsxp<'a> = <expr::Id as Query<expr::Id, Result>>::Inputs<'a>;
impl Query<expr::Id, Result> for expr::Id {
    type Inputs<'a> = (
        &'a mut Ctx,
        &'a mut Codegen,
        &'a Quir,
        &'a mut Mapty,
        &'a mut Maptd,
        &'a mut Maptc,
        &'a mut Mapfn,
        &'a mut Mapbd,
    );

    fn query(map: &mut Mapxp, &id: &expr::Id, inputs: Self::Inputs<'_>) -> Result {
        let this = &inputs.2.items[(inputs.0.id, id)];
        this.kind.codegen(map, this.typ, inputs)
    }
}

impl expr::If {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let smash_b = ctx.lcx.append_basic_block(ctx.val, "smash");
        let pass_b = ctx.lcx.append_basic_block(ctx.val, "smash");
        let post_b = ctx.lcx.append_basic_block(ctx.val, "smash");
        ctx.ifs.push(pass_b);
        let AnyValueEnum::IntValue(cond) = mapxp
            .query(
                self.cond,
                (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd),
            )?
            .0
            .value(ctx)
        else {
            panic!()
        };
        ctx.builder
            .build_conditional_branch(cond, smash_b, pass_b)
            .unwrap();
        let mut cont = false;
        ctx.builder.position_at_end(smash_b);
        let smash_v = mapxp.query(
            self.smash,
            (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd),
        );
        let smash_v = match smash_v {
            Ok((smash_v, b)) => {
                let v = Ok((smash_v.value(ctx), b));
                ctx.builder.build_unconditional_branch(post_b).unwrap();
                cont = true;
                v
            }
            Err(e) => Err(e),
        };
        ctx.builder.position_at_end(pass_b);
        let pass_v = mapxp.query(
            self.pass,
            (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd),
        );
        let pass_v = match pass_v {
            Ok((pass_v, b)) => {
                let v = Ok((pass_v.value(ctx), b));
                ctx.builder.build_unconditional_branch(post_b).unwrap();
                cont = true;
                v
            }
            Err(e) => Err(e),
        };

        ctx.builder.position_at_end(post_b);
        if !cont {
            ctx.builder.build_unreachable().unwrap();
            return Err(smash_v.unwrap_err().min(pass_v.unwrap_err()));
        }

        let (l, p, q) = match (smash_v, pass_v) {
            (Ok((p, b)), Ok((q, d))) => (2, (cg.anyval_to_basic(p), b), (cg.anyval_to_basic(q), d)),
            (Ok((p, b)), _) => (
                1,
                (cg.anyval_to_basic(p), b),
                (ctx.lcx.const_struct(&[], false).into(), pass_b),
            ),
            (_, Ok((q, d))) => (
                1,
                (cg.anyval_to_basic(q), d),
                (ctx.lcx.const_struct(&[], false).into(), smash_b),
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
        phi_v.as_basic_value();
        Ok((LocalValue::Value(phi_v.into()), post_b))
    }
}

impl expr::Call {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        if let expr::Kind::Builtin(b) = quir.items[(ctx.id, self.func)].kind {
            b.codegen_real(
                mapxp,
                (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd),
                self.args,
            )
        } else {
            let func_v = match mapxp
                .query(
                    self.func,
                    (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd),
                )?
                .0
                .value(ctx)
            {
                AnyValueEnum::FunctionValue(v) => v,
                _ => panic!(),
            };
            let args = mapxp
                .query(
                    self.args,
                    (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd),
                )?
                .0
                .value(ctx);
            let res = ctx
                .builder
                .build_call(func_v, &[cg.anyval_to_basic(args).into()], "call")
                .unwrap();
            Ok((
                LocalValue::Value(res.as_any_value_enum()),
                ctx.builder.get_insert_block().unwrap(),
            ))
        }
    }
}

impl expr::Builtinkind {
    pub fn codegen(&self, _: &mut Mapxp, _: quir::Type, _: Inputsxp<'_>) -> Result {
        panic!("builtin shouldn't call codegen!")
    }
    pub fn codegen_real(
        &self,
        mapxp: &mut Mapxp,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
        args_id: expr::Id,
    ) -> Result {
        let mut args = Vec::with_capacity(7);
        match quir.items[(ctx.id, args_id)].kind {
            expr::Kind::Tuple(ref t) => {
                for &f in &t.0 {
                    let f = mapxp
                        .query(f, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
                        .0
                        .value(ctx);
                    args.push(f);
                }
            }
            _ => {
                let a = mapxp
                    .query(args_id, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
                    .0
                    .value(ctx);
                args.push(a);
            }
        }
        let name = self.spelling();
        let bl = &mut ctx.builder;
        use expr::Builtinkind::*;
        Ok((
            LocalValue::Value(match self {
                Add => bl
                    .build_int_add(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                Sub => bl
                    .build_int_sub(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                Mul => bl
                    .build_int_mul(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                Div => bl
                    .build_int_unsigned_div(
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                Mod => bl
                    .build_int_unsigned_rem(
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                And => bl
                    .build_and(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                Or => bl
                    .build_or(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                Xor => bl
                    .build_xor(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                ShLeft => bl
                    .build_left_shift(args[0].into_int_value(), args[1].into_int_value(), name)
                    .unwrap()
                    .into(),
                ShRight => bl
                    .build_right_shift(
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        false,
                        name,
                    )
                    .unwrap()
                    .into(),
                Not => bl.build_not(args[0].into_int_value(), name).unwrap().into(),
                Negate => bl
                    .build_int_neg(args[0].into_int_value(), name)
                    .unwrap()
                    .into(),
                CmpEq => bl
                    .build_int_compare(
                        IntPredicate::EQ,
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                CmpNE => bl
                    .build_int_compare(
                        IntPredicate::NE,
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                CmpGt => bl
                    .build_int_compare(
                        IntPredicate::UGT,
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                CmpLt => bl
                    .build_int_compare(
                        IntPredicate::ULT,
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                CmpGE => bl
                    .build_int_compare(
                        IntPredicate::UGE,
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                CmpLE => bl
                    .build_int_compare(
                        IntPredicate::ULE,
                        args[0].into_int_value(),
                        args[1].into_int_value(),
                        name,
                    )
                    .unwrap()
                    .into(),
                PtrToInt => bl
                    .build_ptr_to_int(args[0].into_pointer_value(), ctx.lcx.i64_type(), name)
                    .unwrap()
                    .into(),
                IntToPtr => bl
                    .build_int_to_ptr(
                        args[0].into_int_value(),
                        ctx.lcx.ptr_type(AddressSpace::default()),
                        name,
                    )
                    .unwrap()
                    .into(),
                Syscall => {
                    let asm_t = ctx
                        .lcx
                        .i64_type()
                        .fn_type(&[ctx.lcx.i64_type().into(); 7], false);
                    let asm_v = ctx.lcx.create_inline_asm(
                        asm_t,
                        "syscall".into(),
                        "=r,{rax},{rdi},{rsi},{rdx},{r8},{r9},{r10}".into(),
                        true,
                        false,
                        Some(InlineAsmDialect::Intel),
                        false,
                    );
                    bl.build_indirect_call(
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
            }),
            bl.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Tuple {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        ty: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let expr_ids = &self.0;
        let AnyTypeEnum::StructType(ty) = mapty.query((ty.kind, None), (cg, quir, maptd)) else {
            panic!()
        };
        let mut comma = ty.get_undef().into();
        for (ix, &id) in expr_ids.iter().enumerate() {
            let v = mapxp
                .query(id, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
                .0
                .value(ctx);
            let v = cg.anyval_to_basic(v);
            comma = ctx
                .builder
                .build_insert_value(comma, v, ix as u32, "tuple")
                .unwrap();
        }
        Ok((
            LocalValue::Value(comma.as_any_value_enum()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Loop {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        ty: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let ty = mapty.query((ty.kind, None), (cg, quir, maptd));
        let ty = cg.anytype_to_basic(ty);
        let post_v = ctx.builder.build_alloca(ty, "&loop_slot").unwrap();
        let loop_b = ctx.lcx.append_basic_block(ctx.val, "loop");
        let post_b = ctx.lcx.append_basic_block(ctx.val, "loop_end");
        ctx.loops.push((post_b, post_v));
        ctx.builder.build_unconditional_branch(loop_b).unwrap();
        ctx.builder.position_at_end(loop_b);
        let v = mapxp.query(self.0, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd));
        if Err(Control::Return) == v {
            return v;
        }
        if v.is_ok() {
            ctx.builder.build_unconditional_branch(loop_b).unwrap();
        }
        ctx.builder.position_at_end(post_b);
        ctx.loops.pop();
        let v = ctx.builder.build_load(ty, post_v, "*loop_slot").unwrap();
        Ok((
            LocalValue::Value(v.into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Bareblock {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let mut out = LocalValue::Value(ctx.lcx.const_struct(&[], false).into());
        for &id in &self.0 {
            out = mapxp
                .query(id, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
                .0;
        }
        Ok((out, ctx.builder.get_insert_block().unwrap()))
    }
}

impl expr::Recall {
    pub fn codegen(
        &self,
        _: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, _): Inputsxp<'_>,
    ) -> Result {
        match self.1.get().unwrap() {
            Left(l) => {
                let binding::Kind::Name(n) = quir.items[(ctx.id, *l)].kind else {
                    panic!()
                };
                Ok((
                    *ctx.lets.get(n.0).unwrap(),
                    ctx.builder.get_insert_block().unwrap(),
                ))
            }
            Right(quir::Id::Function(g)) => Ok((
                LocalValue::Value(if g == &ctx.id {
                    ctx.val.into()
                } else {
                    mapfn.query(*g, (cg, quir, maptd, maptc, mapty)).into()
                }),
                ctx.builder.get_insert_block().unwrap(),
            )),
            Right(quir::Id::Typecase(g)) => Ok((
                LocalValue::Value(maptc.query(*g, (cg, quir, mapty, maptd)).into()),
                ctx.builder.get_insert_block().unwrap(),
            )),
            Right(quir::Id::Typedecl(_)) => panic!(),
        }
    }
}

impl expr::Number {
    pub fn codegen(
        &self,
        _: &mut Mapxp,
        _: quir::Type,
        (ctx, _, _, _, _, _, _, _): Inputsxp<'_>,
    ) -> Result {
        let v = ctx
            .lcx
            .i64_type()
            .const_int_from_string(self.0, StringRadix::Decimal)
            .unwrap();
        Ok((
            LocalValue::Value(v.into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::String {
    pub fn codegen(
        &self,
        _: &mut Mapxp,
        _: quir::Type,
        (ctx, _, _, _, _, _, _, _): Inputsxp<'_>,
    ) -> Result {
        let global = ctx
            .builder
            .build_global_string_ptr(self.0, "string_literal")
            .unwrap();
        Ok((
            LocalValue::Value(global.as_pointer_value().into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Bool {
    pub fn codegen(
        &self,
        _: &mut Mapxp,
        _: quir::Type,
        (ctx, _, _, _, _, _, _, _): Inputsxp<'_>,
    ) -> Result {
        let v = if self.0 == "true" { 1 } else { 0 };
        let v = ctx.lcx.custom_width_int_type(1).const_int(v, false);
        Ok((
            LocalValue::Value(v.into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Arguments {
    pub fn codegen(
        &self,
        _: &mut Mapxp,
        _: quir::Type,
        (ctx, _, _, _, _, _, _, _): Inputsxp<'_>,
    ) -> Result {
        Ok((
            LocalValue::Value(ctx.val.get_nth_param(0).unwrap().into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Poison {
    pub fn codegen(&self, _: &mut Mapxp, _: quir::Type, _: Inputsxp<'_>) -> Result {
        todo!("poison barring codegen")
    }
}

impl expr::Let {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let v = mapxp
            .query(self.1, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
            .0
            .value(ctx);
        let v = cg.anyval_to_basic(v);
        mapbd.query(self.0, (ctx, cg, quir, maptd, mapty, v))
    }
}

impl expr::Assign {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let val = mapxp
            .query(self.1, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
            .0
            .value(ctx);
        let val = cg.anyval_to_basic(val);
        let place = mapxp
            .query(self.0, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
            .0;
        let LocalValue::Place(_, ptr) = place else {
            todo!("verifying non-type sema")
        };
        ctx.builder.build_store(ptr, val).unwrap();
        Ok((
            LocalValue::Value(ctx.lcx.const_struct(&[], false).into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl expr::Break {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let v = mapxp
            .query(self.val, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
            .0
            .value(ctx);
        let (block, ptr) = ctx.loops.last().unwrap();
        ctx.builder
            .build_store(*ptr, cg.anyval_to_basic(v))
            .unwrap();
        ctx.builder.build_unconditional_branch(*block).unwrap();
        Err(Control::Break)
    }
}

impl expr::Return {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        let v = mapxp
            .query(self.0, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?
            .0
            .value(ctx);
        ctx.builder
            .build_store(ctx.ret_v, cg.anyval_to_basic(v))
            .unwrap();
        ctx.builder.build_unconditional_branch(ctx.ret_b).unwrap();
        Err(Control::Return)
    }
}

impl expr::Const {
    pub fn codegen(
        &self,
        mapxp: &mut Mapxp,
        _: quir::Type,
        (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd): Inputsxp<'_>,
    ) -> Result {
        mapxp.query(self.0, (ctx, cg, quir, mapty, maptd, maptc, mapfn, mapbd))?;
        Ok((
            LocalValue::Value(ctx.lcx.const_struct(&[], false).into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

pub type Mapbd = Qmap<binding::Id, Result, binding::Id>;
pub type Inputsbd<'a> = <binding::Id as Query<binding::Id, Result>>::Inputs<'a>;
impl Query<binding::Id, Result> for binding::Id {
    type Inputs<'a> = (
        &'a mut Ctx,
        &'a mut Codegen,
        &'a Quir,
        &'a mut Maptd,
        &'a mut Mapty,
        BasicValueEnum<'static>,
    );

    fn query(
        mapbd: &mut Qmap<binding::Id, Result, Self>,
        &id: &binding::Id,
        inp: Self::Inputs<'_>,
    ) -> Result {
        inp.2.items[(inp.0.id, id)].kind.check(mapbd, inp)
    }
}

impl binding::Empty {
    pub fn check(&self, _: &mut Mapbd, (ctx, _, _, _, _, _): Inputsbd<'_>) -> Result {
        Ok((
            LocalValue::Value(ctx.lcx.custom_width_int_type(1).const_all_ones().into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl binding::Name {
    pub fn check(&self, _: &mut Mapbd, (ctx, _, _, _, _, v): Inputsbd<'_>) -> Result {
        let ty = v.get_type();
        let ptr = ctx.builder.build_alloca(ty, "let").unwrap();
        ctx.builder.build_store(ptr, v).unwrap();
        ctx.lets.insert(self.0, LocalValue::Place(ty, ptr));
        Ok((
            LocalValue::Value(ctx.lcx.custom_width_int_type(1).const_all_ones().into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl binding::Tuple {
    pub fn check(
        &self,
        mapbd: &mut Mapbd,
        (ctx, cg, quir, maptd, mapty, v): Inputsbd<'_>,
    ) -> Result {
        let BasicValueEnum::StructValue(v) = v else {
            panic!()
        };
        for (i, &b) in self.0.iter().enumerate() {
            let v = ctx
                .builder
                .build_extract_value(v, i as u32, "destructure_tuple")
                .unwrap();
            // we'd typically need to and all fields' results together
            // but since a field producing false jumps to a different block
            // anyway, this can never actually be anything other than true
            mapbd.query(b, (ctx, cg, quir, maptd, mapty, v))?;
        }
        Ok((
            LocalValue::Value(ctx.lcx.custom_width_int_type(1).const_all_ones().into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}

impl binding::Constructor {
    pub fn check(
        &self,
        mapbd: &mut Mapbd,
        (ctx, cg, quir, maptd, mapty, v): Inputsbd<'_>,
    ) -> Result {
        let &id = self.id.get().unwrap();
        let tdid = quir.items.typecases[id].parent;
        let index = quir.items.typecases[id].index;
        let Typedecl(_, case) = maptd.query(tdid, (cg, quir, mapty));
        let case = case[index];

        let v = v.into_struct_value();
        let tag_v = ctx
            .builder
            .build_extract_value(v, 1, "type.tag")
            .unwrap()
            .into_int_value();
        let tag_t = tag_v.get_type();
        let index_v = tag_t.const_int(index as u64, false);
        let check = ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag_v,
                index_v,
                "type.tag==constructor.tag",
            )
            .unwrap();
        let success_b = ctx
            .lcx
            .append_basic_block(ctx.val, "constructor_check_success");
        let &fail_b = ctx.ifs.last().unwrap();
        ctx.builder
            .build_conditional_branch(check, success_b, fail_b)
            .unwrap();
        ctx.builder.position_at_end(success_b);

        let tail = ctx.builder.build_extract_value(v, 0, "type.tail").unwrap();
        let tail_ptr = ctx
            .builder
            .build_alloca(tail.get_type(), "alloca_type.tail")
            .unwrap();
        let tail_ty = case.get_field_type_at_index(0).unwrap();
        ctx.builder.build_store(tail_ptr, tail).unwrap();
        let tail = ctx
            .builder
            .build_load(tail_ty, tail_ptr, "*(typecase*)type")
            .unwrap();
        let LocalValue::Value(fields) = mapbd
            .query(self.fields, (ctx, cg, quir, maptd, mapty, tail))?
            .0
        else {
            panic!()
        };
        let fields = fields.into_int_value();
        let out = ctx
            .builder
            .build_and(check, fields, "constructor_check&&fields_check")
            .unwrap();
        Ok((
            LocalValue::Value(out.into()),
            ctx.builder.get_insert_block().unwrap(),
        ))
    }
}
