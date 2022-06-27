use "testing.sml";
use "memory.sml";

fun findValue(parse_numexp(a)) = a
| findValue( parse_bexp(true)) = 1
| findValue(parse_bexp(false)) = 0
| findValue(parse_varexp(a)) = Memory.searchMem(a)
| findValue( _ ) = raise uninitializedError

structure Vmc :
sig

val rules : unit-> unit
val toString: unit -> (string list * string list * string list)


end =
struct
fun patternMatcherforprint(a) = 
	case a of
	 parse_varexp(b) => Memory.searchMem(b)
	| parse_numexp(c) =>c;

fun execute_controlStk(stk : mt Stack)=
if empty(stk) then () else
let
 	val x = top(!C)
 	fun nestingCounter() = 
		let
			fun looper(x) = 
				if(top(!C)=parse_SEQ() andalso x=(~1)) then
					let        
				  		val temp = top(!C)
					in
				  		(V:=push(temp,!V);C:=pop(!C))
				end
			      	else if(top(!C)=parse_demarker() andalso x=0) then
					let
				  		val temp = top(!C)
					in
				  		(V:=push(temp,!V);C:=pop(!C);looper(x-1))
				end
				else if(top(!C)=parse_SEQ()) then
					let
				  		val temp = top(!C)
					in
				  	(C:=pop(!C);V:=push(temp,!V);looper(x+1))
				end
			      	else if(top(!C)=parse_demarker()) then
					let
				  		val temp = top(!C)
					in
				  		(C:=pop(!C);V:=push(temp,!V);looper(x-1))
				end
			      	else
					let
				  		val temp = top(!C)
					in
				 		(C:=pop(!C);V:=push(temp,!V);looper(x))
				end
		in
			(looper(0); if empty(!C) then popperhelp2(1) else ())
		end;
in

 case x of
 	parse_demarker() => (nestingCounter())
	| parse_numexp k => (V:=push(parse_numexp k,!V); store1 := push(x,!store1); C:= pop(!C))
	| parse_varexp k => (V:= push(parse_varexp k, !V);  store1 := push(x,!store1);C:= pop(!C))
	| parse_bexp k => (V:= push(parse_bexp k, !V); store1 := push(x,!store1); C:= pop(!C))
	| parse_PLUS () => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) in ((*print(Int.toString(b) ^ "\n");print(Int.toString(e) ^ "\n");*)V:= pop(!V); V:=pop(!V); store1 := push(x,!store1); V:= push(parse_numexp(b+e),!V); C:= pop(!C)) end)
       | parse_MINUS() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) in (V:= pop(!V); V:=pop(!V);C:= pop(!C);  store1 := push(x,!store1);V:= push(parse_numexp(b-e),!V)) end)
       | parse_TIMES()  => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) in (V:= pop(!V); V:=pop(!V); C:= pop(!C);  store1 := push(x,!store1);V:= push(parse_numexp(b*e),!V)) end)
        | parse_DIV() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) in (V:= pop(!V); V:=pop(!V);C:= pop(!C); store1 := push(x,!store1); V:= push(parse_numexp(b div e),!V)) end)
        | parse_MOD() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) in (V:= pop(!V); V:=pop(!V);C:= pop(!C); store1 := push(x,!store1); V:= push(parse_numexp(b mod e),!V)) end)
        | parse_AND()  => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) in (V:= pop(!V); V:=pop(!V);C:= pop(!C); store1 := push(x,!store1); V:= push(parse_numexp(b*e),!V)) end)
        | parse_NOT() => (let val a = top(!V) val b = findValue(a)  in (V:= pop(!V);C:= pop(!C); store1 := push(x,!store1); V:= push(parse_numexp(1-b),!V)) end)
        | parse_OR()  => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) val f = (1-(1-b)*(1-e)) in (V:= pop(!V);C:= pop(!C);  store1 := push(x,!store1);V:=pop(!V); V:= push(parse_numexp(f),!V)) end)
        | parse_UMINUS() => (let val a = top(!V) val b = findValue(a) val c = 0-b in (V:= pop(!V); C:= pop(!C); store1 := push(x,!store1); V:= push(parse_numexp(c),!V)) end)
        | parse_UPLUS(d) => ( C:= pop(!C); V:= push(parse_numexp(d),!V); store1:= push(parse_numexp(d),!store1))
        | parse_GT() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c)val e = findValue(d)fun comp(g,h) = if g > h then 1 else 0 in  (V:=pop(!V);V:=pop(!V);C:= pop(!C); store1 := push(x,!store1); V:= push(parse_numexp(comp(b,e)),!V)) end)
        | parse_GE() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c)val e = findValue(d) fun comp(g,h) = if g >= h then 1 else 0 in  (V:=pop(!V); C:= pop(!C);V:=pop(!V); store1 := push(x,!store1); V:= push(parse_numexp(comp(b,e)),!V)) end)
	|parse_LT() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) fun comp(g,h) = if g < h then 1 else 0 in (V:=pop(!V);C:= pop(!C);V:=pop(!V); store1 := push(x,!store1); V:= push(parse_numexp(comp(b,e)),!V)) end)
	|parse_LE() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) fun comp(g,h) = if g <= h then 1 else 0 in (V:=pop(!V);C:= pop(!C);V:=pop(!V); store1 := push(x,!store1);V:= push(parse_numexp(comp(b,e)),!V)) end)
	|parse_EQ() => (let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) fun comp(g,h) = if g = h then 1 else 0 in (V:=pop(!V);C:= pop(!C);V:=pop(!V); store1 := push(x,!store1);V:= push(parse_numexp(comp(b,e)),!V)) end)
	| parse_NEQ() =>(let val a = top(!V) val b = findValue(a) val c = pop(!V) val d = top(c) val e = findValue(d) fun comp(g,h) = if g <> h then 1 else 0 in (V:=pop(!V);C:= pop(!C); store1 := push(x,!store1); V:=pop(!V); V:= push(parse_numexp(comp(b,e)),!V)) end)

        | parse_ITE() => (let val a = top(!V) val b = findValue(a) in (if b = 0 then (C:= pop(!C);V:= pop(!V) ;popper(1);popperhelp2(1)) else (C:= pop(!C);V := pop(!V);popperhelp(1); popper(1); popperhelp1(1);popperhelp2(1)))end)
        | parse_SET () => (let val a = top(!V) val  parse_varexp(b) = a val c = pop(!V) val d = top(c) val e = findValue(d) in (V:=pop(!V); V:=pop(!V); C:= pop(!C); Memory.insertMem(b,e))end)
        | parse_SEQ () => (C:= pop(!C))
        | parse_WH () => ( let val a = top(!V) val b = findValue(a) in (if b = 0 then (C:= pop(!C);V:= pop(!V) ;popper(1)) else (V := pop(!V); store1ToC(!store1); popperhelp3(1,!V)))end)
        |parse_READ() => ( let val a = top(!V) val parse_varexp(b) =a  val SOME c= TextIO.inputLine(TextIO.stdIn) val SOME d = Int.fromString(c) in( C:= pop(!C);V:= pop(!V);  Memory.insertMem(b,d)  ) end )
        |parse_WRITE () => (let val a = top(!V) val c = patternMatcherforprint(a) val d = Int.toString(c) in (C:= pop(!C); V:= pop(!V);  print(d ^ "\n")) end)
	| _ => raise notFoundError
end;

fun rules() = if empty(!C) then () else execute_controlStk(!C);

fun btostring(true) = "true"
  | btostring(false) = "false";

fun mttoString(x) = 
case x of
	parse_demarker() => "SEQUENCE DEMARKER"
	| parse_numexp k => Int.toString(k)
	| parse_varexp k => k
	| parse_bexp k => btostring(k)
	| parse_PLUS () => "PLUS"
       | parse_MINUS() => "MINUS"
       | parse_TIMES()  => "TIMES"
        | parse_DIV() => "DIV"
        | parse_MOD() => "MOD"
        | parse_AND()  => "AND"
        | parse_NOT() => "NOT"
        | parse_OR()  => "OR"
        | parse_UMINUS() => "UNARY MINUS"
        | parse_UPLUS(d) => Int.toString(d)
        | parse_GT() => "GT"
        | parse_GE() => "GE"
	|parse_LT() => "LT"
	|parse_LE() => "LE"
	|parse_EQ() => "EQ"
	| parse_NEQ() =>"NEQ"

        | parse_ITE() => "ITE"
        | parse_SET () => "SET"
        | parse_SEQ () => "SEQ"
        | parse_WH () => "WH"
        |parse_READ() => "READ"
        |parse_WRITE () => "WRITE"
	| _ => raise notFoundError;

fun mtstktostring(stk) = if empty(stk) then [] else mttoString(top(stk)) :: mtstktostring(pop(stk));

fun toString() = (mtstktostring(!V),Memory.memtostr(),mtstktostring(!C));
end;

fun mainex(file) = let val a = While.compile(file) val AST.PROG(x,y) = a val AST.CDSEQ(x,y)=y in postfix(y) end;

fun executefinal()=if empty(!C) then () else (Vmc.rules();executefinal());

fun execute(file) = (mainex(file);executefinal());

val x = Vmc.toString();
val (x,y,z) = x;
x;
y;
z;
