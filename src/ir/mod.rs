use std::collections::BTreeSet;

pub mod terms;

pub struct Unit {
    ret_sig: Vec<Type>,
    instructions: Instructions,
	blocks: Blocks,
}

pub enum Type {
	I64,
	I32,
	I16,
	I8,
	Null,
}

pub struct Instructions(Vec<InstData>);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instruction(usize);
pub enum InstData {}

pub struct Blocks(Vec<BlockData>);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block(usize);
pub struct BlockData {
	signature: Vec<Type>,
	inherits: BTreeSet<Block>,
	span: [Instruction; 2],
	term: Terminator,
}

pub enum Terminator {
    Branch(Block, Box<[Instruction]>),
    If(Instruction, Box<[Terminator; 2]>),
	Ret(Option<Instruction>),
}

impl std::ops::Index<Instruction> for Instructions {
    type Output = InstData;

    fn index(&self, index: Instruction) -> &Self::Output {
        &self.0[index.0]
    }
}
impl std::ops::IndexMut<Instruction> for Instructions {
    fn index_mut(&mut self, index: Instruction) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

impl std::ops::Index<Block> for Blocks {
    type Output = BlockData;

    fn index(&self, index: Block) -> &Self::Output {
        &self.0[index.0]
    }
}
impl std::ops::IndexMut<Block> for Blocks {
    fn index_mut(&mut self, index: Block) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}
