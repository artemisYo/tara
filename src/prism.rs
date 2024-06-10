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
impl TreeNode {
	pub fn set_stref(&mut self, stref: usize) {
		self.kind = match self.kind {
			TreeKind::Name(_) => TreeKind::Name(stref),
			TreeKind::Number(_) => TreeKind::Number(stref),
			TreeKind::TreeString(_) => TreeKind::TreeString(stref),
			o => o,
		};
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
		FileView::new_at(&self.0)
	}
}

trait TreeView<'a> {
	fn new_at(_: &'a [TreeNode]) -> Self;
	fn validate(_: &[TreeNode]);
}

pub struct VecView<'a, T>(
	&'a [TreeNode],
	PhantomData<T>,
);
impl<'a, T: TreeView<'a>> VecView<'a, T> {
	fn get_offset(&self, idx: usize) -> Option<&'a [TreeNode]> {
		let mut offset = 1;
		for _ in 0..idx {
			self.in_bounds(offset)?;
			let cur_node = &self.0[offset];
			// check that vec is homogeneous
			T::validate(&self.0[offset..]);
			offset += cur_node.size;
		}
		self.in_bounds(offset)?;
		Some(&self.0[offset..])
	}
	pub fn get(&self, idx: usize) -> Option<T> {
		// idx refers to the node number
		// not to the idx in &[TreeNode]
		Some(T::new_at(self.get_offset(idx)?))
	}
	fn in_bounds(&self, offset: usize) -> Option<()> {
		if offset >= self.0[0].size {
			return None;
		}
		Some(())
	}
}
impl<'a, T> TreeView<'a> for VecView<'a, T> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice, PhantomData)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Vec);
	}
}

pub struct PairView<'a, T, U>(
	&'a [TreeNode],
	PhantomData<(T, U)>,
);
impl<'a, T, U> PairView<'a, T, U> {
	pub fn first(&self) -> T where T: TreeView<'a> {
		T::new_at(&self.0[1..])
	}
	fn second_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn second(&self) -> U where U: TreeView<'a> {
		U::new_at(self.second_offset())
	}
}
impl<'a, T, U> TreeView<'a> for PairView<'a, T, U> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice, PhantomData)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Pair);
	}
}

pub type Params<'a> = VecView<'a, PairView<'a, Name, Name>>;
pub struct Func<'a>(&'a [TreeNode]);
impl<'a> Func<'a> {
	pub fn name(&self) -> Name {
		Name::new_at(&self.0[1..])
	}
	pub fn params(&self) -> Params<'a> {
		Params::new_at(&self.0[2..])
	}
	fn ret_offset(&self) -> &'a [TreeNode] {
		&self.0[2 + self.0[2].size..]
	}
	pub fn ret(&self) -> Option<Name> {
		let ret = self.ret_offset();
		(ret[0].kind != TreeKind::Void).opt()?;
		Some(Name::new_at(ret))
	}
	fn body_offset(&self) -> &'a [TreeNode] {
		let ret = self.ret_offset();
		&ret[ret.size..]
	}
	pub fn body(&self) -> Block<'a> {
		Block::new_at(self.body_offset())
	}
}
impl<'a> TreeView<'a> for Func<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Func);
	}
}

pub type Statements<'a> = VecView<'a, Statement<'a>>;
pub struct Block<'a>(&'a [TreeNode]);
impl<'a> Block<'a> {
	pub fn stmts(&self) -> Statements<'a> {
		Statements::new_at(&self.0[1..])
	}
	fn tail_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn tail(&self) -> Option<Expr<'a>> {
		let tail = self.tail_offset();
		(tail[0].kind != TreeKind::Void).opt()?;
		Some(Expr::new_at(tail))
	}
}
impl<'a> TreeView<'a> for Block<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Block);
	}
}

pub struct Statement<'a>(&'a [TreeNode]);
impl<'a> Statement<'a> {
	pub fn as_let(&self) -> Option<Let<'a>> {
		(self.0[0].kind == TreeKind::Let).opt()?;
		Some(Let::new_at(self.0))
	}
	pub fn as_expr(&self) -> Option<Expr<'a>> {
		(self.0[0].kind.is_expr()).opt()?;
		Some(Expr::new_at(self.0))
	}
}
impl<'a> TreeView<'a> for Statement<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind.is_statement());
	}
}

pub struct Expr<'a>(&'a [TreeNode]);
impl<'a> Expr<'a> {
	pub fn as_if_expr(&self) -> Option<IfExpr<'a>> {
		(self.0[0].kind == TreeKind::IfExpr).opt()?;
		Some(IfExpr::new_at(self.0))
	}
	pub fn as_biop_expr(&self) -> Option<BiopExpr<'a>> {
		(self.0[0].kind == TreeKind::BiopExpr(Op::Less)).opt()?;
		Some(BiopExpr::new_at(self.0))
	}
	pub fn as_func_call(&self) -> Option<Funccall<'a>> {
		(self.0[0].kind ==TreeKind::Funccall).opt()?;
		Some(Funccall::new_at(self.0))
	}
	pub fn as_name(&self) -> Option<Name> {
		(self.0[0].kind == TreeKind::Name(0)).opt()?;
		Some(Name::new_at(self.0))
	}
	pub fn as_number(&self) -> Option<Number> {
		(self.0[0].kind ==TreeKind::Number(0)).opt()?;
		Some(Number::new_at(self.0))
	}
	pub fn as_string(&self) -> Option<TreeString> {
		(self.0[0].kind == TreeKind::TreeString(0)).opt()?;
		Some(TreeString::new_at(self.0))
	}
}
impl<'a> TreeView<'a> for Expr<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind.is_expr());
	}
}

pub struct Let<'a>(&'a [TreeNode]);
impl<'a> Let<'a> {
	pub fn name(&self) -> Name {
		Name::new_at(&self.0[1..])
	}
	pub fn initializer(&self) -> Expr<'a> {
		Expr::new_at(&self.0[2..])
	}
}
impl<'a> TreeView<'a> for Let<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Let);
	}
}

pub struct IfExpr<'a>(&'a [TreeNode]);
impl<'a> IfExpr<'a> {
	pub fn cond(&self) -> Expr<'a> {
		Expr::new_at(&self.0[1..])
	}
	fn then_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn then(&self) -> Expr<'a> {
		Expr::new_at(self.then_offset())
	}
	fn else_offset(&self) -> &'a [TreeNode] {
		let then = self.then_offset();
		&then[then[0].size..]
	}
	pub fn else_(&self) -> Option<Expr<'a>> {
		let else_ = self.else_offset();
		(else_[0].kind != TreeKind::Void).opt()?;
		Some(Expr::new_at(else_))
	}
}
impl<'a> TreeView<'a> for IfExpr<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::IfExpr);
	}
}

pub struct BiopExpr<'a>(&'a [TreeNode]);
impl<'a> BiopExpr<'a> {
	pub fn op(&self) -> Op {
		match self.0[0].kind {
			TreeKind::BiopExpr(o) => o,
			_ => unreachable!(),
		}
	}
	pub fn rhs(&self) -> Expr<'a> {
		Expr::new_at(&self.0[1..])
	}
	fn lhs_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn lhs(&self) -> Expr<'a> {
		Expr::new_at(self.lhs_offset())
	}
}
impl<'a> TreeView<'a> for BiopExpr<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::BiopExpr(Op::Less));
	}
}

pub type Args<'a> = VecView<'a, Expr<'a>>;
pub struct Funccall<'a>(&'a [TreeNode]);
impl<'a> Funccall<'a> {
	pub fn base(&self) -> Expr<'a> {
		Expr::new_at(&self.0[1..])
	}
	fn args_offset(&self) -> &'a [TreeNode] {
		&self.0[1 + self.0[1].size..]
	}
	pub fn args(&self) -> Args<'a> {
		Args::new_at(self.args_offset())
	}
}
impl<'a> TreeView<'a> for Funccall<'a> {
	fn new_at(slice: &'a [TreeNode]) -> Self {
		Self::validate(slice);
		Self(slice)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Funccall);
	}
}

pub struct Name(usize);
impl<'a> TreeView<'a> for Name {
	fn new_at(slice: &[TreeNode]) -> Self {
		Self::validate(slice);
		let TreeKind::Name(n) = slice[0].kind
		else { unreachable!() };
		Self(n)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Name(0));
	}
}
pub struct Number(usize);
impl<'a> TreeView<'a> for Number {
	fn new_at(slice: &[TreeNode]) -> Self {
		Self::validate(slice);
		let TreeKind::Number(n) = slice[0].kind
		else { unreachable!() };
		Self(n)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::Number(0));
	}
}
pub struct TreeString(usize);
impl<'a> TreeView<'a> for TreeString {
	fn new_at(slice: &[TreeNode]) -> Self {
		Self::validate(slice);
		let TreeKind::TreeString(n) = slice[0].kind
		else { unreachable!() };
		Self(n)
	}
	fn validate(slice: &[TreeNode]) {
		assert!(slice[0].kind == TreeKind::TreeString(0));
	}
}
