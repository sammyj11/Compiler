use "while_ast.sml";

open AST

datatype stackElem = expressions of exp
| integers of int
| strings of string 
