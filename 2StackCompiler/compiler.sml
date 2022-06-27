
use "while_ast.sml";

structure While:
sig
	val compile: string -> AST.exp
end =
struct
	exception WhileError;
	fun compile (file: string) : WhileParser.result =
		let val inStream = TextIO.openIn file;
		val inputLn : int -> string = fn n =>
			if TextIO.endOfStream inStream
			then ""
			else TextIO.inputN (inStream,n);
		val printError: string * int * int -> unit = fn (msg,line,col) =>
			print (file ^ " " ^ Int.toString line ^ "." ^ Int.toString col ^ ": " ^ msg ^ "\n");
		val (tree,_) = WhileParser.parse(15,(WhileParser.makeLexer inputLn file),printError,file) handle WhileParser.ParseError => raise WhileError
		
		val _ = TextIO.closeIn inStream;
	in
		tree
	end
end;




