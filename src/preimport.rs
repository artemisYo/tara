use crate::{prescan::OpsId, ModuleId, Query, Tara};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ImportId(ModuleId);
pub struct Preimport<'tara> {
	tara: &'tara mut Tara,
}
impl Query for Preimport<'_> {
	type Input = OpsId;
	type Id = ImportId;
	type Data<'a> = Preimport<'a>;
	type Output = ();

	fn get(tara: &Tara, i: Self::Id) -> &Self::Output {
		todo!()
	}
}
