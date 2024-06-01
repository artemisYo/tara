mod tokenizer;

fn main() {
	let s = "func lol(args : Args) int {some grouping}";
	let tree = tokenizer::tokenize(s.to_string());
	println!("{:#?}", tree);
}
