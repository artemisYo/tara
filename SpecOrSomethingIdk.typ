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
function   <- "fn" name "(" paramList? ")" typeSig? "->" exprList? ("." | ";")
// the structure here is such, that the last parsed parameter
// is treated specially, as it can be important to easily access
// the last element of such a list for later semantic analysis
paramList  <- (parameter ",")* parameter ","?
parameter  <- name typeSig
typeSig    <- ":" type
type       <- "*"* "["*n name ("[" tpList "]")? ((";" numLit)? "]")*n
tpList     <- (typeParam ",")* typeParam ","?
typeParam  <- type
// here for example the last expr is returned from a function
// unless a semicolon is used to end the block
exprList   <- ((blockExpr ","?) | (normExpr ","))* (blockExpr | normExpr) ","?
blockExpr  <- ifStmt | matchStmt | forStmt
ifStmt     <-
matchStmt  <-
forStmt    <- "for" name "in" normExpr "->" exprList? ("." | ";")
normExpr   <- funcCall | arithmetic | dataDecl | literal | name
literal    <- numLit | strLit | charLit | boolLit
numLit     <- ('0'..'9')+
strLit     <- "\"" (!"\"" ("\\\"" | any))* "\""
charLit    <- "'" (!"'" ("\\'" | any)) "'"
boolLit    <- "true" | "false"
funcCall   <- name "(" exprList? ")"
arithmetic <-
dataDecl   <-
name       <- alpha+
alpha      <- 'a' .. 'Z'
```
== Semantics
