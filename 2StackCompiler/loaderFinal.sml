 
 $/basis.cm (* SML/NJ’s Basis Library. *)
 $/ml-yacc-lib.cm (* Code written by Lucent. *)
 $smlnj/compiler/compiler.cm (* Structure Compiler. *)

use "while_ast.sml"

 use"symboltable.sml"
 use"memory.sml"
 use"stack.sml"
 use"testing.sml"
 use"main.yacc.sml"
 use"main.yacc.sig"
 use"main.lex" (* Lexer rules. *)
 (* Parser rules. *)
 use"glue.sml" (* Build the parser *)
 use"compiler.sml" (* Lex, parse, panic... *)

Control.Print.printLength := 10000; (* set printing parameters so that *)
Control.Print.printDepth := 10000; (* we’ll see all details *)
Control.Print.stringDepth := 10000; (* and strings *)
