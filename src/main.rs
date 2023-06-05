mod parser;
mod tokenizer;
use tokenizer::Tokenstream;

fn main() {
    let s = std::fs::read_to_string(std::path::Path::new("./test2.txt")).unwrap();
    let stream = Tokenstream::new(&s);
    let input = stream.stack();
    //let r = parser::Program::parse(input);
    //println!("{:#?}", r);
}
