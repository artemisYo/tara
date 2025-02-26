use crate::Provenance;

#[derive(Debug, Clone, Copy)]
pub struct Token<'s> {
    pub kind: Tokenkind,
    pub loc: Provenance,
    pub text: &'s str,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tokenkind {
    Comment,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Semicolon,
    Colon,
    Slash,
    Ellipsis,
    Func,
    Return,
    Loop,
    Break,
    If,
    Else,
    Let,
    Mut,
    Type,
    As,
    Operator,
    Equals,
    Import,
    Underscore,
    Bool,
    String,
    Number,
    Name,
}

impl Tokenkind {
    pub const fn spelling(self) -> Option<&'static str> {
        Some(match self {
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::OpenBrace => "{",
            Self::CloseBrace => "}",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::Slash => "/",
            Self::Ellipsis => "...",
            Self::Func => "func",
            Self::Return => "return",
            Self::Loop => "loop",
            Self::Break => "break",
            Self::If => "if",
            Self::Else => "else",
            Self::Let => "let",
            Self::Mut => "mut",
            Self::Type => "type",
            Self::As => "as",
            Self::Operator => "operator",
            Self::Equals => "=",
            Self::Import => "import",
            Self::Underscore => "_",
            Self::Bool | Self::String | Self::Number | Self::Name | Self::Comment => return None,
        })
    }
}
