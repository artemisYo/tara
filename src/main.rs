mod parser2;
//use parser::{parse_programm, Str};
mod sema;

fn main() {
    let _s = std::fs::read_to_string(std::path::Path::new("./test.txt")).unwrap();
    //let input = Str::new(&s);
    //let r = parse_programm(input);
    //println!("{:#?}", r);
    //if let Err(e) = r {
    //    e.print_loc();
    //}
}
