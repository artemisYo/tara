use crate::{
    parser::{protocol, typedef},
    tokenizer::Tokenstack,
};

use super::{
    func::{self, Function},
    implements::{self, Implement},
    protocol::Protocol,
    typedef::TypeDef,
    PRes,
};

#[derive(Debug)]
pub struct Root {
    funcs: Vec<Function>,
    protos: Vec<Protocol>,
    impls: Vec<Implement>,
    types: Vec<TypeDef>,
}

pub fn parse(mut input: Tokenstack) -> PRes<Root> {
    let mut out = Root {
        funcs: vec![],
        protos: vec![],
        impls: vec![],
        types: vec![],
    };
    loop {
        if let Ok((s, a)) = func::parse(input) {
            input = s;
            out.funcs.push(a);
        } else if let Ok((s, b)) = implements::parse(input) {
            input = s;
            out.impls.push(b);
        } else if let Ok((s, c)) = protocol::parse(input) {
            input = s;
            out.protos.push(c);
        } else if let Ok((s, d)) = typedef::parse(input) {
            input = s;
            out.types.push(d);
        } else {
            break;
        }
    }
    Ok((input, out))
}
