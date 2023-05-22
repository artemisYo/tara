= Tara
== Syntax
Very basic snippet to establish tara's syntactical goals.
```tara
fn main(args: *[str]): int ->
	for a in args -> 
		println(a);
	0.
```
As such some central guidelines are:
```peg
programm   <- function*
function   <- "fn" name "(" list(parameter)? ")" typeSig? block
block      <- "->" exprList? ("." | ";")
// the structure here is such, that the last parsed parameter
// is treated specially, as it can be important to easily access
// the last element of such a list for later semantic analysis
parameter  <- name typeSig
typeSig    <- ":" type
type       <- "*"* "["*n name ("[" list(typeParam) "]")? ((";" numLit)? "]")*n
typeParam  <- type
// here for example the last expr is returned from a function
// unless a semicolon is used to end the block
exprList   <- ((pipeLine ",") | (blockExpr ","?) | (normExpr ","))* (pipeLine | blockExpr | normExpr)
blockExpr  <- ifStmt | matchStmt | forStmt | "(" exprList ")"
ifStmt     <- "if" exprList block ("else" block)?
matchStmt  <- "match" exprList matchPatt+
matchPatt  <- "case" (name | exprList) block
forStmt    <- "for" name "in" normExpr block
normExpr   <- funcCall | arithmetic | dataAssign
	    | dataDecl | numRange | literal | name
pipeLine   <- (blockExpr | normExpr) ("|>" funcCall)+
numRange   <- (numLit | name) ".." (numLit | name)
literal    <- numLit | strLit | charLit | boolLit
numLit     <- ('0'..'9')+
strLit     <- "\"" (!"\"" ("\\\"" | any))* "\""
charLit    <- "'" (!"'" ("\\'" | any)) "'"
boolLit    <- "true" | "false"
funcCall   <- (name ".")* name "(" list(pipeLine | blockExpr | normExpr)? ")"
arithmetic <-
dataDecl   <- ("let" | "mut") name typeSig? "=" (pipeLine | blockExpr | normExpr)
dataAssign <- name "=" (pipeLine | blockExpr | normExpr)
name       <- alpha+ | "_"
alpha      <- 'a' .. 'Z'
list(x)    <- (x ",")* x ","?
```
== Semantics
