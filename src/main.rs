mod norm_expressions;
pub use norm_expressions::*;
mod block_expressions;
pub use block_expressions::*;
mod macros;
pub use macros::*;

use std::ops::Deref;

type Result<'a> = std::result::Result<(AST<'a>, Str<'a>), ParseError<'a>>;

#[derive(Debug)]
pub enum ParseError<'a> {
    // primitives
    EndOfString,
    TodoError,
    Expected(&'static str),
    InSet(&'static dyn Set<char>),
    NotInSet(&'static dyn Set<char>),
    // Wrappers
    Meta {
        origin: &'static dyn Origin,
        loc: Str<'a>,
        err: Box<Self>,
    },
    Multiple {
        origin: &'static dyn Origin,
        err: Vec<Self>,
    },
    Trace {
        origin: &'static dyn Origin,
        err: Box<Self>,
    },
}

impl<'a> ParseError<'a> {
    fn print_loc(&self) {
        match self {
            Self::Meta {
                origin: _,
                loc,
                err: _,
            } => {
                println!("{:?}", loc.string.lines().nth(loc.pos.0).unwrap());
                println!("{}^", "~".repeat(loc.pos.1));
            }
            Self::Trace { origin: _, err } => {
                err.print_loc();
            }
            Self::Multiple { origin: _, err } => {
                for e in err {
                    e.print_loc();
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug)]
pub enum AST<'a> {
    Prog(Vec<Self>),
    Func {
        name: Box<Self>,
        parameters: Option<Box<Self>>,
        ret_type: Option<Box<Self>>,
        block: Box<Self>,
    },
    Block {
        body: Option<Box<Self>>,
        returns: bool,
    },
    ForStmt {
        ident: Box<Self>,
        source: Box<Self>,
        block: Box<Self>,
    },
    IfStmt {
        condition: Box<Self>,
        smash: Box<Self>,
        pass: Option<Box<Self>>,
    },
    MatchStmt {
        source: Box<Self>,
        patterns: Vec<Self>,
    },
    MatchPatt {
        pattern: Box<Self>,
        smash: Box<Self>,
    },
    Parameter {
        name: Box<Self>,
        type_sig: Box<Self>,
    },
    DataDecl {
        name: Box<Self>,
        source: Box<Self>,
        type_sig: Option<Box<Self>>,
        mutable: bool,
    },
    DataAssign {
        dest: Box<Self>,
        source: Box<Self>,
    },
    ExprList {
        body: Option<Vec<Self>>,
        last: Box<Self>,
    },
    Type {
        name: Box<Self>,
        params: Option<Box<Self>>,
        array: Option<Vec<Option<Box<Self>>>>,
        refd: u8,
    },
    PipeLine {
        start: Box<Self>,
        line: Vec<Self>,
    },
    FuncCall {
        prefix: Vec<Self>,
        name: Box<Self>,
        args: Option<Box<Self>>,
    },
    NumRange {
        start: Box<Self>,
        end: Box<Self>,
    },
    List {
        list: Vec<Self>,
    },
    Name(&'a str),
    Number(&'a str),
    String(&'a str),
    Char(&'a str),
    Bool(&'a str),
}

#[derive(Clone, Copy, Debug)]
pub struct Str<'a> {
    string: &'a str,
    index: usize,
    pos: (usize, usize),
}
impl<'a> Str<'a> {
    fn new(string: &'a str) -> Self {
        Self {
            string,
            index: 0,
            pos: (0, 0),
        }
    }
    fn advance(&mut self, x: usize) {
        if self.index + x > self.string.len() {
            panic!("Str advanced to out of bounds!");
        }
        self.pos.0 += self.string[self.index..self.index + x]
            .chars()
            .map(|c| if c == '\n' { 1 } else { 0 })
            .sum::<usize>();
        if let Some(i) = self.string[self.index..self.index + x].rfind('\n') {
            self.pos.1 = x - i - 1;
        } else {
            self.pos.1 += x;
        }
        self.index += x;
    }
    fn trim_start(&mut self) {
        self.advance(self.chars().take_while(|c| c.is_whitespace()).count());
    }
    fn get_until(&self, i: usize) -> Option<&str> {
        self.deref().get(..i)
    }
    fn check(&mut self, start: &str) -> bool {
        if self.starts_with(start) {
            self.advance(start.len());
            self.trim_start();
            true
        } else {
            false
        }
    }
    fn empty(&self) -> bool {
        self.index == self.string.len()
    }
}

impl<'a> Deref for Str<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.string[self.index..]
    }
}

// programm   <- function*
pub fn parse_programm(input: Str) -> Result {
    let mut acc = vec![];
    let mut head = input;
    while !head.empty() {
        match parse_function(head) {
            Ok((a, s)) => {
                head = s;
                acc.push(a);
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
    return Ok((AST::Prog(acc), head));
}

// function   <- "fn" name "(" list(parameter) ")" typeSig? block
pub fn parse_function(mut input: Str) -> Result {
    orig!(Function);
    let name;
    let block;
    let mut parameters = None;
    let mut ret_type = None;
    if !input.check("fn") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("fn")),
        });
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            name = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    if !input.check("(") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("(")),
        });
    }
    if let Ok((a, s)) = parse_list(input, parse_parameter) {
        input = s;
        parameters = Some(Box::new(a));
    }
    if !input.check(")") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected(")")),
        });
    }
    if let Ok((a, s)) = parse_type_sig(input) {
        input = s;
        ret_type = Some(Box::new(a));
    }
    match parse_block(input) {
        Ok((a, s)) => {
            input = s;
            block = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    Ok((
        AST::Func {
            name,
            parameters,
            ret_type,
            block,
        },
        input,
    ))
}

// block      <- "->" exprList? ("." | ";")
pub fn parse_block(mut input: Str) -> Result {
    orig!(Block);
    let mut returns = true;
    let mut body = None;
    if !input.check("->") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("->")),
        });
    }
    if let Ok((a, s)) = parse_expr_list(input) {
        input = s;
        body = Some(Box::new(a));
    }
    if !input.check(".") {
        returns = false;
        if !input.check(";") {
            return Err(ParseError::Multiple {
                origin,
                err: vec![
                    ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected(".")),
                    },
                    ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected(";")),
                    },
                ],
            });
        }
    }
    Ok((AST::Block { body, returns }, input))
}

// name       <- alpha+ | "_"
pub fn parse_name(mut input: Str) -> Result {
    orig!(Name);
    charset!(Alpha, |c: &char| c.is_ascii_alphabetic());
    let i = input.chars().take_while(|c| set.contains(c)).count();
    if i == 0 {
        if input.check("_") {
            return Ok((AST::Name("_"), input));
        } else {
            return Err(ParseError::Meta {
                origin,
                loc: input,
                err: Box::new(ParseError::NotInSet(set)),
            });
        }
    }
    // for some god forsaken reason I cannot borrow the underlying
    // string unless its done like this
    let name = &input.string[input.index..input.index + i];
    input.advance(i);
    input.trim_start();
    Ok((AST::Name(name), input))
}

// parameter  <- name typeSig
pub fn parse_parameter(mut input: Str) -> Result {
    orig!(Parameter);
    let name;
    let type_sig;
    match parse_name(input) {
        Ok((a, s)) => {
            name = Box::new(a);
            input = s;
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    match parse_type_sig(input) {
        Ok((a, s)) => {
            type_sig = Box::new(a);
            input = s;
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    Ok((AST::Parameter { name, type_sig }, input))
}

// typeSig    <- ":" type
pub fn parse_type_sig(mut input: Str) -> Result {
    orig!(TypeSig);
    if !input.check(":") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected(":")),
        });
    }
    parse_type(input)
}

// type       <- "*"* "["*n name ("[" list(typeParam) "]")? ((";" numLit)? "]")*n
pub fn parse_type(mut input: Str) -> Result {
    orig!(Type);
    let mut ref_depth = 0;
    let mut array_depth = 0;
    let mut params = None;
    let mut array = vec![];
    let name;
    while input.check("*") {
        ref_depth += 1;
    }
    while input.check("[") {
        array_depth += 1;
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            name = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    if input.check("[") {
        match parse_list(input, parse_type_param) {
            Ok((a, s)) => {
                input = s;
                params = Some(Box::new(a));
            }
            Err(e) => {
                return Err(ParseError::Trace {
                    origin,
                    err: Box::new(e),
                })
            }
        }
        if !input.check("]") {
            return Err(ParseError::Meta {
                origin,
                loc: input,
                err: Box::new(ParseError::Expected("]")),
            });
        }
    }
    for _ in 0..array_depth {
        array.push(None);
        if input.check(";") {
            match parse_num_lit(input) {
                Ok((a, s)) => {
                    input = s;
                    *array.last_mut().unwrap() = Some(Box::new(a));
                }
                Err(e) => {
                    return Err(ParseError::Trace {
                        origin,
                        err: Box::new(e),
                    })
                }
            }
        }
        if !input.check("]") {
            return Err(ParseError::Meta {
                origin,
                loc: input,
                err: Box::new(ParseError::Expected("]")),
            });
        }
    }
    let array = if array.is_empty() { None } else { Some(array) };
    Ok((
        AST::Type {
            name,
            params,
            array,
            refd: ref_depth,
        },
        input,
    ))
}

// typeParam  <- type
pub fn parse_type_param(input: Str) -> Result {
    orig!(TypeParam);
    match parse_type(input) {
        Err(e) => Err(ParseError::Trace {
            origin,
            err: Box::new(e),
        }),
        o => o,
    }
}

// exprList   <- ((pipeLine ",") | (blockExpr ","?) | (normExpr ","))* (pipeLine | blockExpr | normExpr)
pub fn parse_expr_list(mut input: Str) -> Result {
    orig!(ExprList);
    let mut acc = vec![];
    let last;
    while let Ok((a, s)) = if let Ok((tree, mut string)) = parse_pipe_line(input) {
        if !string.check(",") {
            Err(())
        } else {
            Ok((tree, string))
        }
    } else if let Ok((tree, mut string)) = parse_block_expr(input) {
        string.check(",");
        Ok((tree, string))
    } else if let Ok((tree, mut string)) = parse_norm_expr(input) {
        if !string.check(",") {
            Err(())
        } else {
            Ok((tree, string))
        }
    } else {
        Err(())
    } {
        input = s;
        acc.push(a);
    }
    match parse_pipe_line(input) {
        Ok((a, s)) => {
            input = s;
            last = Box::new(a);
        }
        Err(e) => match parse_block_expr(input) {
            Ok((a, s)) => {
                input = s;
                last = Box::new(a);
            }
            Err(f) => match parse_norm_expr(input) {
                Ok((a, s)) => {
                    input = s;
                    last = Box::new(a);
                }
                Err(g) => {
                    return Err(ParseError::Multiple {
                        origin,
                        err: vec![e, f, g],
                    });
                }
            },
        },
    }
    let body = if acc.is_empty() { None } else { Some(acc) };
    Ok((AST::ExprList { body, last }, input))
}

// literal    <- numLit | strLit | charLit | boolLit
pub fn parse_literal(input: Str) -> Result {
    orig!(Literal);
    let mut err_stack = vec![];
    match parse_num_lit(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_str_lit(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_char_lit(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_bool_lit(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    Err(ParseError::Multiple {
        origin,
        err: err_stack,
    })
}

// numLit     <- ('0'..'9')+
pub fn parse_num_lit(mut input: Str) -> Result {
    orig!(NumLit);
    charset!(Digit, |x: &char| x.is_ascii_digit());
    let i = input.chars().take_while(|c| set.contains(c)).count();
    if i == 0 {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::NotInSet(set)),
        });
    }
    // for some god forsaken reason I cannot borrow the underlying
    // string unless its done like this
    let num = &input.string[input.index..input.index + i];
    input.advance(i);
    input.trim_start();
    Ok((AST::Number(num), input))
}

// strLit     <- "\"" (!"\"" ("\\\"" | any))* "\""
pub fn parse_str_lit(mut input: Str) -> Result {
    orig!(StrLit);
    let string;
    if !input.check("\"") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("\"")),
        });
    }
    let mut escaped = false;
    let mut i = 0;
    for c in input.chars() {
        if c == '\\' {
            escaped = true;
        }
        if c == '"' && !escaped {
            break;
        }
        i += 1;
    }
    string = &input.string[input.index..input.index + i];
    input.advance(i);
    if !input.check("\"") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("\"")),
        });
    }
    Ok((AST::String(string), input))
}

// charLit    <- "'" (!"'" ("\\'" | any)) "'"
pub fn parse_char_lit(mut input: Str) -> Result {
    orig!(CharLit);
    let char;
    if !input.check("'") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("'")),
        });
    }
    char = match &input.string[input.index..] {
        c if c.starts_with("\\'") => &c[..2],
        c if !c.starts_with("'") => &c[..1],
        _ => {
            return Err(ParseError::Meta {
                origin,
                loc: input,
                err: Box::new(ParseError::TodoError),
            });
        }
    };
    if !input.check("'") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("'")),
        });
    }
    Ok((AST::Char(char), input))
}

// boolLit    <- "true" | "false"
pub fn parse_bool_lit(mut input: Str) -> Result {
    orig!(BoolLit);
    let b = match &input.string[input.index..] {
        b if b.starts_with("true") => {
            input.advance(4);
            &b[..4]
        }
        b if b.starts_with("false") => {
            input.advance(5);
            &b[5..]
        }
        _ => {
            return Err(ParseError::Multiple {
                origin,
                err: vec![
                    ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected("true")),
                    },
                    ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected("false")),
                    },
                ],
            });
        }
    };
    input.trim_start();
    Ok((AST::Bool(b), input))
}

// pipeLine   <- (blockExpr | normExpr) ("|>" funcCall)+
pub fn parse_pipe_line(mut input: Str) -> Result {
    orig!(PipeLine);
    let start;
    let mut line = vec![];
    match parse_block_expr(input) {
        Ok((a, s)) => {
            input = s;
            start = Box::new(a);
        }
        Err(e) => match parse_norm_expr(input) {
            Ok((a, s)) => {
                input = s;
                start = Box::new(a);
            }
            Err(f) => {
                return Err(ParseError::Multiple {
                    origin,
                    err: vec![e, f],
                });
            }
        },
    }
    if !input.check("|>") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("|>")),
        });
    }
    match parse_func_call(input) {
        Ok((a, s)) => {
            input = s;
            line.push(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    while let Ok((a, s)) = if input.check("|>") {
        match parse_func_call(input) {
            Err(_) => Err(()),
            Ok((a, s)) => Ok((a, s)),
        }
    } else {
        Err(())
    } {
        input = s;
        line.push(a);
    }
    Ok((AST::PipeLine { start, line }, input))
}

// list(x)    <- (x ",")* x ","?
fn parse_list(mut input: Str, x: fn(Str) -> Result) -> Result {
    orig!(List);
    let mut list = vec![];
    while let Ok((a, s)) = match x(input) {
        Ok((a, mut s)) => {
            if !s.check(",") {
                Err(())
            } else {
                Ok((a, s))
            }
        }
        Err(_) => Err(()),
    } {
        input = s;
        list.push(a);
    }
    match x(input) {
        Ok((a, s)) => {
            input = s;
            list.push(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    input.check(",");
    Ok((AST::List { list }, input))
}

fn main() {
    let input =
        //Str::new("fn main(args: *[str]): int ->\n\tfor a in args ->\n\t\tprintln(a);\n\t0.");
    Str::new("fn main(args: *[str]): int ->\n\tprintln(fib(5)),\n\t0.\n\nfn fib(count: int): Vec[int] ->\n\tmut acc: Vec[int] = vec(),\n\tmut a = 1,\n\tmut b = 1,\n\tfor _ in 0..count ->\n\t\tacc.push(b),\n\t	b = a,\n\t\ta = add(a, b);\n\tacc.");
    //Str::new("fn main(args: *[str]): int ->\n\tprintln(fib(5)),\n\t0.\n\nfn fib(count: int): Vec[int] ->\n\tmut acc: Vec[int] = vec(),\n\tmut a = 1,\n\tmut b = 1,\n\tfor.");
    let r = parse_programm(input);
    println!("{:#?}", r);
    if let Err(e) = r {
        e.print_loc();
    }
}
