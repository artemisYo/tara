use crate::{ModuleId, Tara};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ImportId(ModuleId);
pub struct Preimport<'tara> {
    tara: &'tara mut Tara,
}

