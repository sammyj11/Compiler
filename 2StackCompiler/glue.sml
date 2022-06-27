structure WhileLrVals = WhileLrValsFun(structure Token = LrParser.Token);
structure WhileLex = WhileLexFun(structure Tokens = WhileLrVals.Tokens);
structure WhileParser =
	  JoinWithArg(structure LrParser = LrParser
     	       structure ParserData = WhileLrVals.ParserData
     	       structure Lex = WhileLex);
