use crate::tokenizer::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum AstTypes {
    Func,
}
pub enum Ast<'a> {
    Node {
        node_type: AstTypes,
        children: &'a [Self],
    },
    Leaf(&'a Token<'a>),
}
impl Ast<'_> {
    pub fn occurences<'a>(&'a self, t: &AstTypes) -> Vec<&'a Self> {
        match self {
            Self::Node {
                node_type: n,
                children: c,
            } => {
                if n == t {
                    return vec![self];
                }
                return c.iter().flat_map(|s| s.occurences(t).into_iter()).collect();
            }
            Self::Leaf(_) => {
                return vec![];
            }
        }
    }
}
