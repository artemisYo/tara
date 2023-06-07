use crate::{
    parser::{protocol, typedef},
    tokenizer::Tokenstack,
};

use super::{
    implements::{self, Implement},
    protocol::Protocol,
    typedef::TypeDef,
    PRes,
};

#[derive(Debug)]
pub struct Root {
    funcs: (),
    protos: Vec<Protocol>,
    impls: Vec<Implement>,
    types: Vec<TypeDef>,
}

pub fn parse(mut input: Tokenstack) -> PRes<Root> {
    let mut out = Root {
        funcs: (),
        protos: vec![],
        impls: vec![],
        types: vec![],
    };
    loop {
        if let Ok((s, a)) = implements::parse(input) {
            input = s;
            out.impls.push(a);
        } else if let Ok((s, a)) = protocol::parse(input) {
            input = s;
            out.protos.push(a);
        } else if let Ok((s, b)) = typedef::parse(input) {
            input = s;
            out.types.push(b);
        } else {
            break;
        }
    }
    Ok((input, out))
}
