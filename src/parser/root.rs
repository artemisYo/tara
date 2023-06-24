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
        match func::parse(input) {
            Ok((s, a)) => {
                input = s;
                out.funcs.push(a);
                continue;
            }
            Err(_) => {}
        }       
        match implements::parse(input) {
            Ok((s, b)) => {
                input = s;
             out.impls.push(b);
              continue;
            }
            Err(_) => {}
        }
        match protocol::parse(input) {
            Ok((s, c)) => {
                input = s;
                out.protos.push(c);
                continue;
            }
            Err(_) => {}
        }
        match typedef::parse(input) {
            Ok((s, d)) => {
                input = s;
                out.types.push(d);
            }
            Err(_) => {break;},
        }
    }
    Ok((input, out))
}
