mod lexer;
mod ast;
use ast::{syntax, exec};

fn main() {
    let file = std::fs::read_to_string("./test.tara").unwrap();
    println!("----Src----");
    println!("{:?}", file);
    let ts = lexer::tokenize(&file).unwrap();
    println!("----TkS----");
    println!("{:?}", ts);
    let st = syntax::parse(&ts).unwrap();
    println!("----AST----");
    println!("{:?}", st);
    let rs = exec::run(st);
    println!("----Run----");
    println!("{:?}", rs);
}
