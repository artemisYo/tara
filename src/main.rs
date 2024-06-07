mod tokenizer;
mod errors;
mod parser;

fn main() {
	let s = std::fs::read_to_string("test.tara").unwrap();
	let mut errs = errors::ErrorQueue::new();
	let tree = tokenizer::tokenize(&mut errs, &s).unwrap();
	errs.print();
	println!("{:#?}", tree);
	let ast = parser::parse(tree.view()).unwrap();
	println!("{:#?}", ast);
}
