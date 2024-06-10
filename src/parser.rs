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

struct Parser {
	buffer: Vec<TreeNode>,
}
impl Parser {
	fn with_scope<T: ValidatorBase, A>(&mut self, f: impl FnOnce(&mut Self, Validator<T, A>) -> Option<(TreeKind, T)>) -> Option<T> {
		let idx = self.buffer.len();
		self.buffer.push(TreeNode::default());
		let (kind, pow) = f(self, Validator::new())?;
		let size = self.buffer.len() - idx;
		self.buffer[idx].kind = kind;
		self.buffer[idx].size = size;
		Some(pow)
	}
	// File = (&:func Func)* ε
	fn file(&mut self, mut tt: TokenView) -> Option<TokenView> {
		while !tt.is_empty() {
			match tt.peek().unwrap().kind {
				TokenKind::Func => {
					let rest = self.func(tt)?;
					tt = rest;
				},
				_ => todo!("error handling"),
			}
		}
		tt.is_empty().opt()?;
		Some(tt)
	}
	// Func = :func Name :paren{Params} (:arrRight Name)? :brace{Block}
	fn func(&mut self, mut tt: TokenView) -> Option<TokenView> {
		self.with_scope(|this, validator| {
			tt.expect(TokenKind::Func)?;
			let (rest, validator) = this.name(tt, validator)?;
			tt = rest;
			let validator = this.params(
				tt.children_if(TokenKind::Paren(0))?,
				validator,
			)?;
			tt.next();
			let validator = if tt.peek()?.kind == TokenKind::ArrRight {
				tt.next();
				let (rest, validator) = this.name(tt, validator)?;
				tt = rest;
				validator
			} else {
				this.void(validator)
			};
			// block on children
			// tt.next()
			// return
		});
	}
	// Params = (Name :colon Name :comma)* (Name :colon Name)? ε
	fn params<T, N: Naturals, Head, Tail>(
		&mut self,
		mut tt: TokenView,
		to_validate: Validator<T, (Head, Tail)>
	) -> Option<Validator<T, Tail>>
	where Head: Find<ParamsBase, N> {
		let pow_token = self.with_scope(|this, validator| {
			while !tt.is_empty() {
				let _ = this.with_scope(|this, validator| {
					let (rest, validator) = this.name(tt, validator)?;
					rest.expect(TokenKind::Colon)?;
					let (rest, validator) = this.name(rest, validator)?;
					tt = rest;
					Some((TreeKind::Pair, validator.finish()))
				})?;
				if tt.peek()?.kind != TokenKind::Comma {
					break;
				}
			}
			tt.is_empty().opt()?;
			Some((TreeKind::Vec, validator.finish()))
		})?;
		Some(to_validate.apply(pow_token))
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
	// 		self.buffer.push(TreeNode {
	// 			size: 1,
	// 			kind: TreeKind::Void,
	// 		});
	// 	}
	// 	tt.is_empty().opt()?;
	// 	Some(tt)
	// }
	fn block<T, N: Naturals, Head, Tail>(
		&mut self,
		mut tt: TokenView,
		to_validate: Validator<T, (Head, Tail)>,
	) -> Option<(TokenView, Validator<T, Tail>)>
	where Head: Find<BlockBase, N> {
		let stmts_pow = self.with_scope(|this, validator| {
			while !tt.is_empty() {
				let (rest, validator) = this.statement(tt, validator);
			}
		})?;
	}
	fn name<T, N: Naturals, Head, Tail>(
		&mut self,
		mut tt: TokenView,
		to_validate: Validator<T, (Head, Tail)>
	) -> Option<(TokenView, Validator<T, Tail>)>
	where Head: Find<NameBase, N> {
		let pow_token = self.with_scope(|this, validator| {
			let name = tt.expect(TokenKind::Name(0))?.string_idx()?;
			Some((TreeKind::Name(name), validator.finish()))
		})?;
		let valid = to_validate.apply(pow_token);
		Some((tt, valid))
	}
	fn void<T, N: Naturals, Head, Tail>(
		&mut self,
		to_validate: Validator<T, (Head, Tail)>
	) -> Validator<T, Tail>
	where Head: Find<VoidBase, N> {
		self.buffer.push(TreeNode {
			size: 1, kind: TreeKind::Void,
		});
		to_validate.apply(VoidBase)
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
