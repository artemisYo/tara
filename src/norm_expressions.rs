use crate::*;

// normExpr   <- funcCall | arithmetic | dataAssign | dataDecl | numRange | literal | name
pub fn parse_norm_expr(input: Str) -> Result {
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
    match parse_data_assign(input) {
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
    match parse_num_range(input) {
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

// numRange   <- (numLit | name)".." (numLit | name)
fn parse_num_range(mut input: Str) -> Result {
    orig!(NumRange);
    let start;
    let end;
    match parse_num_lit(input) {
        Ok((a, s)) => {
            input = s;
            start = Box::new(a);
        }
        Err(e) => match parse_name(input) {
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
    if !input.check("..") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("..")),
        });
    }
    match parse_num_lit(input) {
        Ok((a, s)) => {
            input = s;
            end = Box::new(a);
        }
        Err(e) => match parse_name(input) {
            Ok((a, s)) => {
                input = s;
                end = Box::new(a);
            }
            Err(f) => {
                return Err(ParseError::Multiple {
                    origin,
                    err: vec![e, f],
                });
            }
        },
    }
    Ok((AST::NumRange { start, end }, input))
}

// funcCall   <- (name ".")* name "(" list(pipeLine | blockExpr | normExpr)? ")"
pub fn parse_func_call(mut input: Str) -> Result {
    orig!(FuncCall);
    let name;
    let mut args = None;
    let mut prefix = vec![];
    while let Ok((a, s)) = match parse_name(input) {
        Ok((a, mut s)) => {
            if !s.check(".") {
                Err(())
            } else {
                Ok((a, s))
            }
        }
        Err(_) => Err(()),
    } {
        input = s;
        prefix.push(a);
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
    if let Ok((a, s)) = parse_list(input, |i: Str| {
        parse_pipe_line(i)
            .or(parse_block_expr(i))
            .or(parse_norm_expr(i))
    }) {
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
    Ok((AST::FuncCall { prefix, name, args }, input))
}

pub fn parse_arithmetic(input: Str) -> Result {
    orig!(Arithmetic);
    return Err(ParseError::Meta {
        origin,
        loc: input,
        err: Box::new(ParseError::TodoError),
    });
}

// dataDecl   <- ("let" | "mut") name typeSig? "=" (pipeLine | blockExpr | normExpr)
pub fn parse_data_decl(mut input: Str) -> Result {
    orig!(DataDecl);
    let name;
    let source;
    let mut type_sig = None;
    let mut mutable = false;
    if !input.check("let") {
        if input.check("mut") {
            mutable = true;
        } else {
            return Err(ParseError::Multiple {
                origin,
                err: vec![
                    ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected("let")),
                    },
                    ParseError::Meta {
                        origin,
                        loc: input,
                        err: Box::new(ParseError::Expected("mut")),
                    },
                ],
            });
        }
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
    if let Ok((a, s)) = parse_type_sig(input) {
        input = s;
        type_sig = Some(Box::new(a));
    }
    if !input.check("=") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("=")),
        });
    }
    match parse_pipe_line(input) {
        Ok((a, s)) => {
            input = s;
            source = Box::new(a);
        }
        Err(e) => match parse_block_expr(input) {
            Ok((a, s)) => {
                input = s;
                source = Box::new(a);
            }
            Err(f) => match parse_norm_expr(input) {
                Ok((a, s)) => {
                    input = s;
                    source = Box::new(a);
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
    Ok((
        AST::DataDecl {
            name,
            source,
            type_sig,
            mutable,
        },
        input,
    ))
}

// dataAssign <- name "=" (pipeLine | blockExpr | normExpr)
pub fn parse_data_assign(mut input: Str) -> Result {
    orig!(DataAssign);
    let dest;
    let source;
    match parse_name(input) {
        Ok((a, s)) => {
            input = s;
            dest = Box::new(a);
        }
        Err(e) => {
            return Err(ParseError::Trace {
                origin,
                err: Box::new(e),
            });
        }
    }
    if !input.check("=") {
        return Err(ParseError::Meta {
            origin,
            loc: input,
            err: Box::new(ParseError::Expected("=")),
        });
    }
    match parse_pipe_line(input) {
        Ok((a, s)) => {
            input = s;
            source = Box::new(a);
        }
        Err(e) => match parse_block_expr(input) {
            Ok((a, s)) => {
                input = s;
                source = Box::new(a);
            }
            Err(f) => match parse_norm_expr(input) {
                Ok((a, s)) => {
                    input = s;
                    source = Box::new(a);
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
    Ok((AST::DataAssign { dest, source }, input))
}
