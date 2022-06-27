# Compiler
Made a compiler with 2 stack architecture (Turing machine) from a self made AST Generator of the WHILE LANGUAGE(turing complete)

The 2StackCompiler folder has the lex and yacc files for parsing the program and generating the AST. This AST is then converted to a postfix notation stack and stored in the stack
This forms the first stack and the second stack is used for evaluation(called the V stack while the first one was called the C stack). A fixed size memory is also needed
Hence we get the name VMC machine

A few test cases have also been provided which show that program display correct output on right inputs and flags errors on wrong inputs
