// Deref <- "*" Executable
// Refer <- "&" Executable

pub mod deref {
    use crate::{
        expect,
        parser::{
            executable::{self, Executable},
            patterns::Pattern,
            PRes, Traceable,
        },
        tokenizer::{Token, Tokenstack},
    };

    use super::super::Expression;

    #[derive(Debug)]
    pub struct Deref(Box<dyn Executable>);
    impl Pattern for Deref {
        fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
            self
        }
    }
    impl Expression for Deref {
        fn as_expression(self: Box<Self>) -> Box<dyn Expression> {
            self
        }
    }
    impl Executable for Deref {
        fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
            self
        }
    }
    const NAME: &'static str = "Deref";
    pub fn parse(mut input: Tokenstack) -> PRes<Deref> {
        expect!(input, Token::Star);
        let (s, o) = executable::parse(input).trace(NAME)?;
        input = s;
        Ok((input, Deref(o)))
    }
}
pub mod refer {
    use crate::{
        expect,
        parser::{
            executable::{self, Executable},
            patterns::Pattern,
            PRes, Traceable,
        },
        tokenizer::{Token, Tokenstack},
    };

    use super::super::Expression;

    #[derive(Debug)]
    pub struct Refer(Box<dyn Executable>);
    impl Pattern for Refer {
        fn as_pattern(self: Box<Self>) -> Box<dyn Pattern> {
            self
        }
    }
    impl Expression for Refer {
        fn as_expression(self: Box<Self>) -> Box<dyn Expression> {
            self
        }
    }
    impl Executable for Refer {
        fn as_executable(self: Box<Self>) -> Box<dyn Executable> {
            self
        }
    }
    const NAME: &'static str = "Refer";
    pub fn parse(mut input: Tokenstack) -> PRes<Refer> {
        expect!(input, Token::Ampersand);
        let (s, o) = executable::parse(input).trace(NAME)?;
        input = s;
        Ok((input, Refer(o)))
    }
}
