use std::marker::PhantomData;

pub trait BoolOpt {
	fn opt(self) -> Option<()>;
}
impl BoolOpt for bool {
	fn opt(self) -> Option<()> {
		if self {
			Some(())
		} else  {
			None
		}
	}
}

#[derive(Debug)]
pub struct File(Vec<TreeNode>);

#[derive(Debug)]
pub struct TreeNode {
	size: usize,
	kind: TreeKind
}

#[derive(Debug)]
pub enum TreeKind {
	Vec,
	Pair,
	Func,
	Block,
	Let,
	IfExpr,
	BiopExpr(Op),
	Funccall,
	Name(usize),
	Number(usize),
	TreeString(usize),
	Void,
}
impl PartialEq for TreeKind {
	fn eq(&self, other: &TreeKind) -> bool {
		std::mem::discriminant(self) == std::mem::discriminant(other)
	}
}
impl Eq for TreeKind {}
impl TreeKind {
	fn is_statement(&self) -> bool {
		*self == Self::Let || self.is_expr()
	}
	fn is_expr(&self) -> bool {
		match self {
			Self::IfExpr |
			Self::BiopExpr(_) |
			Self::Funccall |
			Self::Name(_) |
			Self::Number(_) |
			Self::TreeString(_) => true,
			_ => false
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
	Less, More, Equal,
	LessEq, MoreEq,
	Add, Sub,
}

pub type FileView<'a> = VecView<'a, Func<'a>>;
impl File {
	pub fn view<'a>(&'a self) -> FileView<'a> {
		View::new_at(&self.0)
	}
}

trait ViewBase {
	fn is_valid(_: &[TreeNode]) -> bool;
}
trait ValidatorBase {
	type Args;
	fn new() -> Self;
}

pub struct View<'a, T>(&'a [TreeNode], PhantomData<T>);
impl<'a, T: ViewBase> View<'a, T> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		T::is_valid(slice).opt().unwrap();
		Self(slice, PhantomData)
	}
	fn is_valid(&self) -> bool {
		T::is_valid(self.0)
	}
	pub fn cast<V: View<'a, _>>(&self) -> Option<V> {
		let out = V::new_at(self.0);
		out.is_valid().opt()?;
		Some(out)
	}
}

macro_rules! tlist {
	() => { () };
	( $head:tt $(,)? ) => { ($head, ()) };
	( $head:tt , $($tail:tt),+ $(,)? ) => {
		($head, tlist!($($tail),+) )
	};
}

trait Naturals {}
type Zero = ();
type Succ<N> = (N, ());
impl Naturals for Zero {}
impl<N: Naturals> Naturals for Succ<N> {}

trait List {
	type Head; type Tail;
}
impl<T, U> List for (T, U) {
	type Head = T; type Tail = U;
}

trait Find<T, N: Natural> {}
impl<T, U, L> Find<T, Zero> for L 
where L: List<Head = T, Tail = U> {}
impl<F, T, N: Natural, U: Find<F, N>, L> Find<F, Succ<N>> for L 
where L: List<Head = T, Tail = U> {}

type Cons<Head, Tail> = (Head, Tail);
type Nil = ();
pub struct Validator<T, Args>(PhantomData<(T, Args)>);
impl<T, Head, Tail> Validator<T, Cons<Head, Tail>> {
	pub fn apply(self, _: Head) -> Validator<T, Tail> {
		Validator(PhantomData)
	}
}
impl<T: ValidatorBase> Validator<T, T::Args> {
	pub fn new() -> Self {
		Validator(PhantomData)
	}
}
impl<T: ValidatorBase> Validator<T, Nil> {
	pub fn finish(self) -> T {
		T::new()
	}
}

struct VecBase<T>(PhantomData<T>);
pub type VecView<'a, T> = View<'a, VecBase<T>>;
impl<T> ViewBase for Vec<T> {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Vec
	}
}
impl<T> ValidatorBase for VecBase<T> {
	type Args = ();
	fn new() -> Self { Self(PhantomData) }
}
impl<'a, T> VecView<'a, T> {
	fn in_bounds(&self, offset: usize) -> bool {
		offset < self.0[0].size
	}
	fn get_offset(&self, idx: usize) -> Option<&'a [TreeNode]> {
		let mut offset = 1;
		for _ in 0..idx {
			self.in_bounds(offset).opt()?;
			let cur_node = &self.0[offset];
			offset += cur_node.size;
		}
		self.in_bounds(offset).opt()?;
		Some(&self.0[offset..])
	}
	// idx refers to the node number
	// not to the idx in &[TreeNode]
	pub fn get(&self, idx: usize) -> Option<View<'a, T>> {
		Some(View::new_at(self.get_offset(idx)?))
	}
}

struct PairBase<T, U>(PhantomData<(T, U)>);
pub type Pair<'a, T, U> = View<'a, PairBase<T, U>>;
impl<T, U> ViewBase for PairBase<T, U> {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Pair
	}
}
impl<T, U> ValidatorBase for PairBase<T, U> {
	type Args = tlist!(T, U);
	fn new() -> Self { Self(PhantomData) }
}
impl<'a, T, U> Pair<'a, T, U> {
	pub fn first(&self) -> View<'a, T> where T: TreeView {
		View::new_at(&self.0[1..])
	}
	fn second_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn second(&self) -> View<'a, U> where U: TreeView {
		View::new_at(self.second_offset())
	}
}

type ParamsBase = VecBase<PairBase<NameBase, NameBase>>;
pub type Params<'a> = VecView<'a, Pair<'a, Name, Name>>;
struct FuncBase;
pub type Func<'a> = View<'a, Func>;
impl ViewBase for FuncBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Func
	}
}
impl ValidatorBase for FuncBase {
	type Args = tlist!(
		NameBase,
		ParamsBase,
		Option<NameBase>,
		BlockBase,
	);
	fn new() -> Self { Self }
}
impl<'a> Func<'a> {
	pub fn name(&self) -> Name<'a> {
		View::new_at(&self.0[1..])
	}
	pub fn params(&self) -> Params<'a> {
		View::new_at(&self.0[2..])
	}
	fn ret_offset(&self) -> &'a [TreeNode] {
		&self.0[2 + self.0[2].size..]
	}
	pub fn ret(&self) -> Option<Name<'a>> {
		let ret = self.ret_offset();
		(ret[0].kind != TreeKind::Void).opt()?;
		Some(View::new_at(ret))
	}
	fn body_offset(&self) -> &'a [TreeNode] {
		let ret = self.ret_offset();
		&ret[ret.size..]
	}
	pub fn body(&self) -> Block<'a> {
		View::new_at(self.body_offset())
	}
}

type StatementsBase = VecBase<StatementBase>;
pub type Statements<'a> = VecView<'a, Statement<'a>>;
struct BlockBase;
pub type Block<'a> = View<'a, BlockBase>;
impl ViewBase for BlockBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Block
	}
}
impl ValidatorBase for BlockBase {
	type Args = tlist!(StatementsBase, Option<ExprBase>);
	fn new() -> Self { Self }
}
impl<'a> Block<'a> {
	pub fn stmts(&self) -> Statements<'a> {
		View::new_at(&self.0[1..])
	}
	fn tail_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn tail(&self) -> Option<Expr<'a>> {
		let tail = self.tail_offset();
		(tail[0].kind != TreeKind::Void).opt()?;
		Some(View::new_at(tail))
	}
}

pub struct StatementBase;
pub type Statement<'a> = View<'a, StatementBase>;
impl ViewBase for StatementBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind.is_statement()
	}
}
impl ValidatorBase for StatementBase {
	type Args = tlist!(Result<ExprBase, LetBase>);
	fn new() -> Self { Self }
} 

pub struct ExprBase;
pub type Expr<'a> = View<'a, ExprBase>;
impl ViewBase for ExprBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind.is_expr()
	}
}
impl ValidatorBase for ExprBase {
	fn new() -> Self { Self }
}

pub struct LetBase;
pub type Let<'a> = View<'a, LetBase>;
impl ViewBase for LetBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Let
	}
}
impl ValidatorBase for LetBase {
	fn new() -> Self { Self }
}
impl<'a> Let<'a> {
	pub fn name(&self) -> Name<'a> {
		View::new_at(&self.0[1..])
	}
	pub fn initializer(&self) -> Expr<'a> {
		View::new_at(&self.0[2..])
	}
}

pub struct IfExprBase;
pub type IfExpr<'a> = View<'a, IfExprBase>;
impl ViewBase for IfExprBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::IfExpr
	}
}
impl ValidatorBase for IfExprBase {
	fn new() -> Self { Self }
}
impl<'a> IfExpr<'a> {
	pub fn cond(&self) -> Expr<'a> {
		View::new_at(&self.0[1..])
	}
	fn then_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn then(&self) -> Expr<'a> {
		View::new_at(self.then_offset())
	}
	fn else_offset(&self) -> &'a [TreeNode] {
		let then = self.then_offset();
		&then[then[0].size..]
	}
	pub fn else_(&self) -> Option<Expr<'a>> {
		let else_ = self.else_offset();
		(else_[0].kind != TreeKind::Void).opt()?;
		Some(View::new_at(else_))
	}
}

pub struct BiopExprBase;
pub type BiopExpr<'a> = View<'a, BiopExprBase>;
impl ViewBase for BiopExprBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::BiopExpr
	}
}
impl ValidatorBase for BiopExprBase {
	fn new() -> Self { Self }
}
impl<'a> BiopExpr<'a> {
	pub fn op(&self) -> Op {
		match self.0[0].kind {
			TreeKind::BiopExpr(o) => o,
			_ => unreachable!(),
		}
	}
	pub fn rhs(&self) -> Expr<'a> {
		View::new_at(&self.0[1..])
	}
	fn lhs_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn lhs(&self) -> Expr<'a> {
		View::new_at(self.lhs_offset())
	}
}

pub type Args<'a> = VecView<'a, Expr<'a>>;
pub struct FunccallBase;
pub type Funccall<'a> = View<'a, FunccallBase>;
impl<'a> Funccall<'a> {
	pub fn base(&self) -> Expr<'a> {
		View::new_at(&self.0[1..])
	}
	fn args_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn args(&self) -> Args<'a> {
		View::new_at(self.args_offset())
	}
}
impl ViewBase for FunccallBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Funccall
	}
}
impl ValidatorBase for FunccallBase {
	fn new() -> Self { Self }
}

pub struct NameBase;
pub type Name<'a> = View<'a, NameBase>;
impl ViewBase for NameBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Name(0)
	}
}
impl ValidatorBase for NameBase {
	fn new() -> Self { Self }
}
pub struct NumberBase;
pub type Number<'a> = View<'a, NumberBase>;
impl ViewBase for NumberBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::Number(0)
	}
}
impl ValidatorBase for NumberBase {
	fn new() -> Self { Self }
}
pub struct TreeStringBase;
pub type TreeString<'a> = View<'a, TreeStringBase>;
impl ViewBase for TreeStringBase {
	fn is_valid(slice: &[TreeNode]) -> bool {
		slice[0].kind == TreeKind::TreeString(0)
	}
}
impl ValidatorBase for TreeStringBase {
	fn new() -> Self { Self }
}
