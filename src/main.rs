mod tokenizer;
mod errors;

fn main() {
	let s = "func lol{(args : Args} 0int5 {some grouping}";
	let mut errs = errors::ErrorQueue::new();
	let tree = tokenizer::tokenize(&mut errs, s.to_string());
	errs.print();
	println!("{:#?}", tree);
}
