use std::ops::Deref;

type Result<'a> = std::result::Result<(AST<'a>, Str<'a>), ParseError<'a>>;

trait Set<T: std::fmt::Debug>: std::fmt::Debug {
    fn contains(&self, _: &T) -> bool;
}
impl<T: std::fmt::Debug + PartialEq> Set<T> for [T] {
    fn contains(&self, e: &T) -> bool {
        self.contains(&e)
    }
}

trait Origin: std::fmt::Debug {}

macro_rules! orig {
    ($name:ident) => {
        #[derive(Debug)]
        struct $name;
        impl Origin for $name {}
        #[allow(non_upper_case_globals)]
        const origin: &dyn Origin = &$name;
    };
}
macro_rules! charset {
    ($name:ident) => {
        #[derive(Debug)]
        struct $name;
        #[allow(non_upper_case_globals)]
        const set: &dyn Set<char> = &$name;
    };
    ($name:ident, $pred:expr) => {
        #[derive(Debug)]
        struct $name;
        impl Set<char> for $name {
            fn contains(&self, o: &char) -> bool {
                $pred(o)
            }
        }
        #[allow(non_upper_case_globals)]
        const set: &dyn Set<char> = &$name;
    };
}

#[derive(Debug)]
enum ParseError<'a> {
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

#[derive(Debug)]
enum AST<'a> {
    Prog(Vec<Self>),
    Func {
        name: Box<Self>,
        parameters: Option<Box<Self>>,
        ret_type: Option<Box<Self>>,
        body: Option<Box<Self>>,
        returns: bool,
    },
    ForStmt {
        ident: Box<Self>,
        source: Box<Self>,
        body: Option<Box<Self>>,
        returns: bool,
    },
    ParamList {
        body: Option<Vec<Self>>,
        last: Box<Self>,
    },
    Parameter {
        name: Box<Self>,
        type_sig: Box<Self>,
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
    TypeParams {
        body: Vec<Self>,
    },
    FuncCall {
        name: Box<Self>,
        args: Option<Box<Self>>,
    },
    Name(&'a str),
    Number(&'a str),
    String(&'a str),
    Char(&'a str),
    Bool(&'a str),
}

#[derive(Clone, Copy, Debug)]
struct Str<'a> {
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
fn parse_programm(input: Str) -> Result {
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

// function   <- "fn" name "(" paramList? ")" typeSig? "->" exprList? ("." | ";")
fn parse_function(mut input: Str) -> Result {
    orig!(Function);
    let name;
    let mut parameters = None;
    let mut ret_type = None;
    let mut body = None;
    let mut does_return = true;
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
    if let Ok((a, s)) = parse_param_list(input) {
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
        does_return = false;
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
    Ok((
        AST::Func {
            name,
            parameters,
            ret_type,
            body,
            returns: does_return,
        },
        input,
    ))
}

// name       <- alpha+
fn parse_name(mut input: Str) -> Result {
    orig!(Name);
    charset!(Alpha, |c: &char| c.is_ascii_alphabetic());
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
    let name = &input.string[input.index..input.index + i];
    input.advance(i);
    input.trim_start();
    Ok((AST::Name(name), input))
}

// paramList  <- (parameter ",")* parameter ","?
fn parse_param_list(mut input: Str) -> Result {
    orig!(ParamList);
    let mut params = vec![];
    let last;
    while let Ok((a, s)) = match parse_parameter(input) {
        Ok((tree, mut head)) => {
            if !head.check(",") {
                Err(())
            } else {
                Ok((tree, head))
            }
        }
        Err(_) => Err(()),
    } {
        input = s;
        params.push(a);
    }
    match parse_parameter(input) {
        Ok((a, s)) => {
            input = s;
            last = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    input.check(",");
    let body = if params.is_empty() {
        None
    } else {
        Some(params)
    };
    Ok((AST::ParamList { body, last }, input))
}

// parameter  <- name typeSig
fn parse_parameter(mut input: Str) -> Result {
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
fn parse_type_sig(mut input: Str) -> Result {
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

// type       <- "*"* "["*n name ("[" tpList "]")? ((";" numLit)? "]")*n
fn parse_type(mut input: Str) -> Result {
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
        match parse_tp_list(input) {
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

// tpList     <- (typeParam ",")* typeParam ","?
fn parse_tp_list(mut input: Str) -> Result {
    orig!(TPList);
    let mut body = vec![];
    while let Ok((a, s)) = match parse_type_param(input) {
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
        body.push(a);
    }
    match parse_type_param(input) {
        Ok((a, s)) => {
            input = s;
            body.push(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    input.check(",");
    Ok((AST::TypeParams { body }, input))
}

// typeParam  <- type
fn parse_type_param(input: Str) -> Result {
    orig!(TypeParam);
    match parse_type(input) {
        Err(e) => Err(ParseError::Trace {
            origin,
            err: Box::new(e),
        }),
        o => o,
    }
}

// exprList   <- ((blockExpr ","?) | (normExpr ","))* (blockExpr | normExpr) ","?
fn parse_expr_list(mut input: Str) -> Result {
    orig!(ExprList);
    let mut acc = vec![];
    let last;
    while let Ok((a, s)) = if let Ok((tree, mut string)) = parse_block_expr(input) {
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
    match parse_block_expr(input) {
        Ok((a, s)) => {
            input = s;
            last = Box::new(a);
        }
        Err(e) => match parse_norm_expr(input) {
            Ok((a, s)) => {
                input = s;
                last = Box::new(a);
            }
            Err(f) => {
                return Err(ParseError::Multiple {
                    origin,
                    err: vec![e, f],
                });
            }
        },
    }
    input.check(",");
    let body = if acc.is_empty() { None } else { Some(acc) };
    Ok((AST::ExprList { body, last }, input))
}

// normExpr   <- funcCall | arithmetic | dataDecl | literal | name
fn parse_norm_expr(input: Str) -> Result {
    orig!(NormExpr);
    let mut err_stack = vec![];
    match parse_func_call(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_arithmetic(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_data_decl(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_literal(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_name(input) {
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

// blockExpr  <- ifStmt | matchStmt | forStmt
fn parse_block_expr(input: Str) -> Result {
    orig!(BlockExpr);
    let mut err_stack = vec![];
    match parse_if_stmt(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_match_stmt(input) {
        Err(e) => {
            err_stack.push(e);
        }
        o => {
            return o;
        }
    }
    match parse_for_stmt(input) {
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
fn parse_if_stmt(input: Str) -> Result {
    orig!(IfStmt);
    return Err(ParseError::Meta {
        origin,
        loc: input,
        err: Box::new(ParseError::TodoError),
    });
}
fn parse_match_stmt(input: Str) -> Result {
    orig!(MatchStmt);
    return Err(ParseError::Meta {
        origin,
        loc: input,
        err: Box::new(ParseError::TodoError),
    });
}

// forStmt    <- "for" name "in" normExpr "->" exprList? ("." | ";")
fn parse_for_stmt(mut input: Str) -> Result {
    orig!(ForStmt);
    let ident;
    let source;
    let mut body = None;
    let mut returns = false;
    if !input.check("for") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("for")),
        });
    }
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            ident = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    if !input.check("in") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("in")),
        });
    }
    match parse_norm_expr(input) {
        Ok((a, s)) => {
            input = s;
            source = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
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
    match input.check(".") {
        true => {
            returns = true;
        }
        false => {
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
    }
    Ok((
        AST::ForStmt {
            ident,
            source,
            body,
            returns,
        },
        input,
    ))
}

// funcCall   <- name "(" exprList? ")"
fn parse_func_call(mut input: Str) -> Result {
    orig!(FuncCall);
    let name;
    let mut args = None;
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
    if let Ok((a, s)) = parse_expr_list(input) {
        input = s;
        args = Some(Box::new(a));
    }
    if !input.check(")") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected(")")),
        });
    }
    Ok((AST::FuncCall { name, args }, input))
}

fn parse_arithmetic(input: Str) -> Result {
    orig!(Arithmetic);
    return Err(ParseError::Meta {
        origin,
        loc: input,
        err: Box::new(ParseError::TodoError),
    });
}
fn parse_data_decl(input: Str) -> Result {
    orig!(DataDecl);
    return Err(ParseError::Meta {
        origin,
        loc: input,
        err: Box::new(ParseError::TodoError),
    });
}

// literal    <- numLit | strLit | charLit | boolLit
fn parse_literal(input: Str) -> Result {
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
fn parse_num_lit(mut input: Str) -> Result {
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
fn parse_str_lit(mut input: Str) -> Result {
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
fn parse_char_lit(mut input: Str) -> Result {
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
fn parse_bool_lit(mut input: Str) -> Result {
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

fn main() {
    let input =
        Str::new("fn main(args: *[str]): int ->\n\tfor a in args ->\n\t\tprintln(a);\n\t0.");
    let r = parse_programm(input);
    println!("{:#?}", r);
}
