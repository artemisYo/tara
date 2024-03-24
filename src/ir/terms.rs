use super::{Terminator, Block, Instruction};

pub fn branch(block: Block, args: Box<[Instruction]>) -> Terminator {
	Terminator::Branch(block, args)
}

pub fn ret(arg: impl Into<Option<Instruction>>) -> Terminator {
	Terminator::Ret(arg.into())
}

pub fn do_if(
    condition: Instruction,
    on_true: Terminator,
    on_false: Terminator,
) -> Terminator {
	Terminator::If(condition, Box::new([on_true, on_false]))
}
