use crate::parsing::{Ast, AstTypes};
use crate::tokenizer::{Block, Delimiter, Grouping, Keyword, Literal, Token, TokenStream};

pub enum ParseError {
    FuncExpectName(usize),
}

pub fn parse(mut input: TokenStream) -> Result<Ast, ParseError> {
    match input.peek() {
        Token::Block(_) => {}
        Token::Delimiter(_) => {}
        Token::Keyword(Keyword::Fn) => {
            parse_func(&mut input)?;
        }
        Token::Literal(_) => {}
        _ => todo!(),
    }
    todo!();
}

fn parse_func<'a>(input: &'a mut TokenStream) -> Result<Ast<'a>, ParseError> {
    let name;
    let params;
    let ret;
    let block;
    input.shift();
    if input.peek().expect(Token::Literal(Literal::Name(""))) {
        name = Ast::Leaf(input.peek());
        input.shift();
    } else {
        return Err(ParseError::FuncExpectName(input.pos()));
    }
    params = parse_func_params(input.peek())?;
    input.shift();
    todo!();
    if !input.peek().expect(Token::Delimiter(Delimiter::Minus)) {
        return Err(());
    }
    input.shift();
    if !input.peek().expect(Token::Delimiter(Delimiter::MoreThan)) {
        return Err(());
    }
    input.shift();
    if input.peek().expect(Token::Literal(Literal::Name(""))) {
        ret = Ast::Leaf(input.peek());
        input.shift();
    } else {
        return Err(());
    }
    if input.peek().expect(Token::Block(Block {
        grouping: Grouping::Braces,
        stream: TokenStream::new(0),
    })) {
        block = Ast::Leaf(input.peek());
        input.shift();
    } else {
        return Err(());
    }
    return Ok(Ast::Node {
        node_type: AstTypes::Func,
        children: &[name, params, ret, block],
    });
}

fn parse_func_params(input: &Token<'_>) -> Result<Ast, ParseError> {
    todo!();
}
