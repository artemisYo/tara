use crate::tokenizer::{TokenKind, TokenView, TokenNode};
mod prism;
pub use prism::*;

pub fn parse(tt: TokenView) -> Option<File> {
	let mut parser = Parser {
		buffer: vec![],
	};
	parser.file(tt)?;
	Some(File(parser.buffer))
}

type PRes<'a, T> = Option<(TokenView<'a>, T)>;

struct Parser {
	buffer: Vec<TreeNode>,
}
impl Parser {
	// File = (&:func Func)* ε
	fn file<'a>(&mut self, mut tt: TokenView<'a>) -> Option<TokenView<'a>> {
		while !tt.is_empty() {
			match tt.peek().unwrap().kind {
				TokenKind::Func => {
					let (rest, _) = self.func(tt)?;
					tt = rest;
				},
				_ => todo!("error handling"),
			}
		}
		tt.is_empty().opt()?;
		Some(tt)
	}
	// Func = :func Name :paren{Params} (:arrRight Name)? :brace{Block}
	fn func<'a>(&mut self, mut tt: TokenView<'a>) -> PRes<'a, FuncBase> {
		tt.expect(TokenKind::Func)?;
		let scope = self.new_scope();
		let (mut tt, scope) = self.with_scope(scope, Self::name, tt)?;
		let (_, scope) = self.with_scope(
			scope,
			Self::params,
			tt.children_if(TokenKind::Paren(0))?
		)?;
		tt.next();
		let (mut tt, scope) = if tt.peek()?.kind == TokenKind::ArrRight {
			tt.next();
			self.with_scope(scope, Self::name, tt)?
		} else {
			self.with_scope(scope, Self::void, tt)?
		};
		let (_, scope) = self.with_scope(
			scope,
			Self::block,
			tt.children_if(TokenKind::Brace(0))?
		)?;
		tt.next();
		let pow = self.close_scope(scope, TreeKind::Func);
		Some((tt, pow))
	}
	// Params = (Name :colon Name :comma)* (Name :colon Name)? ε
	fn params<'a>(&mut self, mut tt: TokenView<'a>) -> PRes<'a, ParamsBase> {
		let scope = self.new_scope();
		while !tt.is_empty() {
			let pairscope = self.new_scope::<PairBase<NameBase, NameBase>>();
			let (mut rest, pairscope) = self.with_scope(pairscope, Self::name, tt)?;
			rest.expect(TokenKind::Colon)?;
			let (rest, pairscope) = self.with_scope(pairscope, Self::name, rest)?;
			tt = rest;
			self.close_scope(pairscope, TreeKind::Pair);
			if tt.peek()?.kind != TokenKind::Comma {
				break;
			}
		}
		tt.is_empty().opt()?;
		let pow = self.close_scope(scope, TreeKind::Vec);
		Some((tt, pow))
	}
	// Block = (Statement :semi)* Expr? ε
	// fn block(&mut self, mut tt: TokenView) -> PRes {
	// 	let vec = self.buffer.len();
	// 	self.buffer.push(TreeNode {
	// 		size: 0,
	// 		kind: TreeKind::Vec,
	// 	});
	// 	while !tt.is_empty() {
	// 		let size = self.buffer.len() - vec;
	// 		tt = self.statement(tt)?;
	// 		if tt.peek()?.kind == TokenKind::Semi {
	// 			tt.next();
	// 		} else {
	// 			self.buffer[vec].size = size;
	// 			break;
	// 		}
	// 	}
	// 	// if Expr? didn't happen
	// 	if self.buffer[vec].size == 0 {
	// 		self.buffer[vec].size = self.buffer.len() - vec;
	// 		self.buffer.push(TreeNode {Pair
	// 			size: 1,
	// 			kind: TreeKind::Void,
	// 		});
	// 	}
	// 	tt.is_empty().opt()?;
	// 	Some(tt)
	// }
	fn block<'a>(&mut self, tt: TokenView<'a>) -> PRes<'a, BlockBase> {
		todo!("block grammar is a bit yikes");
	}
	fn name<'a>(&mut self, mut tt: TokenView<'a>) -> PRes<'a, NameBase> {
		let name = tt.expect(TokenKind::Name(0))?.string_idx()?;
		let pow = self.push_name(name);
		Some((tt, pow))
	}
	fn void<'a>(&mut self, tt: TokenView<'a>) -> PRes<'a, VoidBase> {
		let pow = self.push_void();
		Some((tt, pow))
	}
}

// Statement = (:let :name :eq Expr) / Expr
// Expr = BlockExpr / InlineExpr
// BlockExpr = :if Expr :brace{Block} (:else :brace{Block})?
// InlineExpr = BoolExpr
// BoolOp = :less / :more / :eqEq / :lessEq / :moreEq
// BoolExpr = Arithmetic (BoolOp Arithmetic)*
// Arithmetic = FuncCall ((:plus / :minus) FuncCall)*
// FuncCall = LitExpr (:paren{List(false, true, Expr, :comma)})?
// LitExpr = :name / :number / :string
