#  COL226 While-AST Assignment

-By Samyak Jain
2020CS50667

## Rules to use the program
load the programme using CM.make ("while.cm")
then put in the following commands 
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)

these ensure that proper depth of AST and strings in the stack are printed to the terminal

then to view the AST type While.compile("input.txt")
to view the evaluation type use "VMC.sml" on the terminal
then call the execute(fileName) function with input filename
then call VMC.toString(); to output the V C stacks as wel as the memory




## Description of Assignment

In this assignment I have tried to implement the evaluator for the Abstract syntax tree made by me in the previous assignment. This is done using the VMC machine as described in the assignment in the postfix notation using 2 stacks (V and C) along with memory M implmented as a hashtable

## Other Design Decisions

In the while language, I have implemented the language as specified in the EBNF of notes and grammar was defined afresh in the previous assignement to cover the entire language

the terminals and nonterminals remain as before in the previous assignment.

while_ast has data datatypes for both, constructors of AST as well as for the control stack which contains the postfix form of AST. These new datatypes can be bijected to the earlier datatypes by adding the prefix  < parse_>

I have implemented the VMC structure with the rules defined in it which are given by Prof S Arun kumar in assignment instructions
Then I have made a new function, which pops one instruction from control stack, evaluates it using the rules and then recursively calls itself

In correspondence to rules, we successfully evaluate the entire postfix notation of AST for a given programme written in while language

Following are the contents of each file

a .lex file for lexing 
a .yacc file for writing grammar and making the AST
one sml file which contains the structure for symboltable
one sml file which contains the structure for memory
one sml file which contains the structure of AST for datatypes
one sml file which contains the compiler function
one sml file which contains the glue code
one sml file which contains the structure for FunStack
one .sml whihc contains the VMC structure
one .sml file which contains the postfix function and
 declares the stacks 
one .cm file which contains the compile manager
a .md file which has the readme
an input.txt file containing a sample test case

## Other Implementation Decisions

I have used the grammar specified in the EBNF and implemented a symboltable to type check while making the AST

to accommodate read and write, I have used stdIn and TextIO which are built in

bools have been assinged as 0 for false and 1 for true

## Acknowledgements

HyperNotes given by Prof S arun Kumar
pi project given in http://rogerprice.org/ug/ug.pdf
sml documentation
