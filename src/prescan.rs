use std::num::NonZero;

use crate::{tokens::{Token, Tokenkind}, ModuleId, Tara};

#[derive(Debug, Clone, Copy)]
pub struct Opdef {
	lbp: Option<NonZero<u32>>,
	rbp: Option<NonZero<u32>>,
	spelling: crate::misc::Istr,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct OpsId(ModuleId);
impl OpsId {
	pub fn assume(m: ModuleId) -> Self { Self(m) }
	pub fn module(&self) -> ModuleId { self.0 }
}

fn expect_kinds<'a>(i: &mut impl Iterator<Item = Token<'a>>, k: &[Tokenkind]) -> Token<'a> {
	match i.next() {
		Some(t) if k.contains(&t.kind) => t,
		Some(_t) => todo!("[Error report] Expected tokens [@k] but got token @t at @loc"),
		None => todo!("[Error report] Expected tokens [@k] but got EOF at @loc"),
	}
}

pub fn prescan(
	ctx: &mut Tara,
	m: ModuleId,
) -> OpsId {
	let mut ops = Vec::new();
	let mut tokens = Vec::new();
	let (modules, mut interner) = ctx.split_for_prescan();
	let mut lexer = modules.get_lexer(m);
	while let Some(t) = lexer.next() {
		match t.kind {
			Tokenkind::Comment => continue,
			Tokenkind::Operator => {
				expect_kinds(&mut lexer, &[Tokenkind::OpenParen]);
				let lhs = expect_kinds(&mut lexer, &[Tokenkind::Number, Tokenkind::Underscore]);
				expect_kinds(&mut lexer, &[Tokenkind::Comma]);
				let name = expect_kinds(&mut lexer, &[Tokenkind::Name]);
				expect_kinds(&mut lexer, &[Tokenkind::Comma]);
				let rhs = expect_kinds(&mut lexer, &[Tokenkind::Number, Tokenkind::Underscore]);
				expect_kinds(&mut lexer, &[Tokenkind::CloseParen]);
				let name = interner.intern(name.text);
				let lbp = if lhs.text == "_" { 0 }
				else {
					lhs.text.parse::<u32>().expect("lexer shouldn't allow errors here") + 1
				};
				let rbp = if rhs.text == "_" { 0 }
				else {
					rhs.text.parse::<u32>().expect("lexer shouldn't allow errors here") + 1
				};
				let op = Opdef {
					lbp: NonZero::new(lbp),
					rbp: NonZero::new(rbp),
					spelling: name,
				};
				ops.push(op);
			},
			_ => {
				let text = interner.intern(t.text);
				tokens.push(Token {
					kind: t.kind,
					loc: t.loc,
					text: *text,
				});
				todo!()
			},
		}
	}
	modules.place_ops(m, ops.into_boxed_slice());
	modules.place_tokens(m, tokens.into_boxed_slice());
	OpsId(m)
}
