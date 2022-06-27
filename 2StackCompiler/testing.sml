

use "stack.sml";
use "memory.sml";
open AST


open FunStack




exception uninitializedError
exception notFoundError
exception arrityError
val V : mt Stack ref= ref create;
val C: mt Stack ref = ref create;
val store : mt Stack ref = ref create;
val store1 : mt Stack ref = ref create;
val store2 : mt Stack ref = ref create;


  
fun popper(alpha) =
let 
val abc = top(!V)
in
(V:= pop(!V);
if (abc = parse_demarker() andalso alpha = 2) then ()
else if(abc = parse_demarker()) then (popper(alpha-1))
else if(abc = parse_SEQ() )then (popper(alpha+1))
else popper(alpha))
end;


fun popperhelp(alpha) =
let 
val abc = top(!V)
in
V:= pop(!V);
store := push(abc,!store);
if (abc = parse_demarker() andalso alpha = 2)then ()
else if(abc = parse_demarker() )then (popperhelp(alpha-1))
else if(abc = parse_SEQ()) then (popperhelp(alpha+1))
else popperhelp(alpha)
end;

fun popperhelp1(alpha) =
let 
val abc = top(!store)
in
store:= pop(!store);
V := push(abc,!V);
if (abc = parse_SEQ()  andalso alpha = 2 )then ()
else if(abc = parse_SEQ() )then (popperhelp1(alpha-1))
else if(abc = parse_demarker() )then( popperhelp1(alpha+1))
else popperhelp1(alpha)
end;

fun popperhelp2(alpha) =
let 
val abc = top(!V)
in
V:= pop(!V);
if (abc = parse_demarker() andalso alpha = 2)then ()
else if(abc = parse_demarker() )then (C := push(abc,!C); popperhelp2(alpha-1))
else if(abc = parse_SEQ() andalso alpha = 1) then (popperhelp2(alpha+1))
else if(abc = parse_SEQ()) then (C := push(abc,!C); popperhelp2(alpha+1))
else (C := push(abc,!C);popperhelp2(alpha))
end;

fun popperhelp3(alpha,stk) =
let 
val abc = top(stk)
in
if (abc = parse_demarker() andalso alpha = 2)then ()
else if(abc = parse_demarker() )then (C := push(abc,!C); popperhelp3(alpha-1,pop(stk)))
else if(abc = parse_SEQ() andalso alpha = 1) then (popperhelp3(alpha+1,pop(stk)))
else if(abc = parse_SEQ()) then (C := push(abc,!C); popperhelp3(alpha+1,pop(stk)))
else (C := push(abc,!C);popperhelp3(alpha,pop(stk)))
end;

fun store1ToC(stk)= 
let 
val a = top(stk)
in
case a of 
 parse_numexp k => (C:=push(parse_numexp k,!C);pop(stk) )
	| parse_varexp k => (C:= push(parse_varexp k, !C);pop(stk))
	| parse_bexp k => (C:= push(parse_bexp k, !C);pop(stk))
	| parse_PLUS () => (C:= push(parse_PLUS() , !C); store1ToC(store1ToC(pop(stk))))
       | parse_MINUS() => (C:= push(parse_MINUS() , !C); store1ToC(store1ToC(pop(stk))))
       | parse_TIMES()  => (C:= push(parse_TIMES() , !C); store1ToC(store1ToC(pop(stk))))
        | parse_DIV() => (C:= push(parse_DIV() , !C); store1ToC(store1ToC(pop(stk))))
        | parse_MOD() => (C:= push(parse_MOD() , !C); store1ToC(store1ToC(pop(stk))))
        | parse_AND()  => (C:= push(parse_AND() , !C); store1ToC(store1ToC(pop(stk))))
        | parse_NOT() => (C:= push(parse_NOT() , !C); store1ToC(pop(stk)))
        | parse_OR()  => (C:= push(parse_OR() , !C); store1ToC(store1ToC(pop(stk))))
        | parse_UMINUS() => (C:= push(parse_UMINUS() , !C); store1ToC(pop(stk)))
        | parse_GT() => (C:= push(parse_GT() , !C); store1ToC(store1ToC(pop(stk))))
        | parse_GE() => (C:= push(parse_GE() , !C); store1ToC(store1ToC(pop(stk))))
	|parse_LT() => (C:= push(parse_LT() , !C); store1ToC(store1ToC(pop(stk))))
	|parse_LE() =>(C:= push(parse_LE() , !C); store1ToC(store1ToC(pop(stk))))
	|parse_EQ() => (C:= push(parse_EQ() , !C); store1ToC(store1ToC(pop(stk))))
	| parse_NEQ() =>(C:= push(parse_NEQ() , !C); store1ToC(store1ToC(pop(stk))))
	| _ => raise arrityError 
	end;


exception wrongExpr ;

(*
val a = While.compile("input.txt");

fun postfix(AST(PROG(CDSEQ(decseq,commseq))))=
commseq;

val b = postfix(a);

*)






fun postfix( SEQ(d)) = let val e=rev(d) in (C:=  push(parse_SEQ(), !C);app postfix e; C:= push(parse_demarker(), !C) ) end

|postfix( PLUS(d,e)) =(C:= push(parse_PLUS(),!C);postfix(d);postfix(e))
| postfix( MINUS(d,e)) =(C:= push(parse_MINUS(),!C);postfix(d); postfix(e))
| postfix( TIMES(d,e)) =(C:= push(parse_TIMES(),!C);postfix(d);postfix(e))
| postfix( DIV(d,e)) =(C:= push(parse_DIV(),!C);postfix(d);postfix(e))
|postfix( MOD(d,e)) =(C:= push(parse_MOD(),!C);postfix(d); postfix(e))
| postfix( UMINUS(d) )=(C:= push(parse_UMINUS(),!C);postfix(d))
| postfix( UPLUS(d) )=(C:= push(parse_UPLUS(d),!C))
| postfix( OR(d,e)) = (C:= push(parse_OR(),!C);postfix(d);postfix(e))
| postfix( AND(d,e)) =(C:= push(parse_AND(),!C);postfix(d);postfix(e))
| postfix( GT(d,e)) =(C:= push(parse_GT(),!C);postfix(d) ;postfix(e))
|postfix( LT(d,e)) =(C:= push(parse_LT(),!C);postfix(d);postfix(e))
|postfix( GE(d,e)) =(C:= push(parse_GE(),!C);postfix(d);postfix(e))
|postfix( LE(d,e)) =(C:= push(parse_LE(),!C);postfix(d);postfix(e))
|postfix( EQ(d,e)) =(C:= push(parse_EQ(),!C);postfix(d);postfix(e))
|postfix( NEQ(d,e)) =(C:= push(parse_NEQ(), !C);postfix(d);postfix(e))
| postfix( NOT(d)) =(C:= push(parse_NOT(),!C); postfix(d))
| postfix( ITE(d,e,f)) =(C:= push(parse_ITE(),!C); postfix(d);postfix(e);postfix(f))
| postfix( WH(d,e)) =(C:= push(parse_WH(),!C);postfix(d);postfix(e))  
| postfix( READ(e)) =(C:= push(parse_READ(),!C); postfix(varexp(e)))
| postfix( WRITE(e) )=(C:=push(parse_WRITE(),!C);postfix(e))
| postfix( SET(d,e)) =(C:= push(parse_SET(),!C);C:= push(parse_varexp(d),!C); postfix(e))
| postfix( bexp(e) )=(C:= push(parse_bexp(e),!C))
| postfix( numexp(e) )=(C:= push(parse_numexp(e),!C))
| postfix( varexp(e) )=(C:= push(parse_varexp(e),!C))
| postfix( _ )= raise wrongExpr;



(*

postfix(SEQ
          [SET
             ("A",
              PLUS
                (PLUS
                   (numexp 1,
                    TIMES
                      (MINUS
                         (PLUS (TIMES (numexp 7,numexp 9),numexp 6),
                          DIV (TIMES (numexp 2,numexp 5),numexp 2)),
                       MINUS (PLUS (numexp 7,numexp 9),numexp 3))),
                 MOD (numexp 8,numexp 3))),
           SET
             ("B",
              PLUS
                (MINUS
                   (PLUS (UPLUS 8,numexp 9),TIMES (UPLUS 3,UMINUS (numexp 2))),
                 DIV (PLUS (numexp 7,TIMES (numexp 9,numexp 3)),numexp 2))),
           SET ("C",UPLUS 1),SET ("D",UMINUS (varexp "C")),
           SET
             ("A",
              PLUS (PLUS (PLUS (varexp "A",varexp "B"),varexp "C"),varexp "D")),
           ITE
             (GT (varexp "A",varexp "B"),
              SEQ
                [WH
                   (AND (GT (varexp "A",varexp "B"),GT (varexp "B",numexp 0)),
                    SEQ
                      [SET ("A",MINUS (varexp "A",numexp 1)),
                       WRITE (varexp "A")])],
              SEQ
                [WH
                   (LT (varexp "A",varexp "B"),
                    SEQ
                      [SET ("A",PLUS (varexp "A",numexp 1)),
                       WRITE (varexp "A")])]),WRITE (varexp "A"),
           WRITE (varexp "B"),WRITE (varexp "C"),WRITE (varexp "D"),
           SET
             ("A",
              PLUS (PLUS (PLUS (varexp "A",varexp "B"),varexp "C"),varexp "D")),
           SET ("B",PLUS (PLUS (varexp "B",varexp "C"),varexp "D")),
           SET ("C",PLUS (varexp "C",varexp "D")),
           SET ("D",UMINUS (varexp "D")),WRITE (varexp "A"),
           WRITE (varexp "B"),WRITE (varexp "C"),WRITE (varexp "D")]
);
C;
*)



(*postfix( SEQ
          [SET ("A",UPLUS 1),SET ("B",numexp 1),
           ITE
             (GT (varexp "A",varexp "B"),SEQ [SET ("A",numexp 2)],
              SEQ [SET ("A",numexp 3)])]); *)
              
(* postfix(SEQ
          [SET ("A",UPLUS 1),SET ("B",UPLUS 10),
           WH
             (LE (varexp "A",varexp "B"),
              SEQ [SET ("A",PLUS (varexp "A",UPLUS 1))])]); *)
              
 (* postfix( SEQ
          [SET ("A",UPLUS 20),SET ("B",UPLUS 40),
           WH
             (AND (LT (varexp "A",varexp "B"),GT (varexp "B",UPLUS 0)),
              SEQ
                [SET ("A",PLUS (varexp "A",UPLUS 2)),
                 WH
                   (AND (GT (varexp "A",UPLUS 0),GT (varexp "B",UPLUS 2)),
                    SEQ [SET ("B",MINUS (varexp "B",UPLUS 1))])])]); *)
(*postfix(SEQ
          [SET
             ("A",
              PLUS
                (PLUS
                   (numexp 1,
                    TIMES
                      (MINUS
                         (PLUS (TIMES (numexp 7,numexp 9),numexp 6),
                          DIV (TIMES (numexp 2,numexp 5),numexp 2)),
                       MINUS (PLUS (numexp 7,numexp 9),numexp 3))),
                 MOD (numexp 8,numexp 3))),
           SET
             ("B",
              PLUS
                (MINUS
                   (PLUS (UPLUS 8,numexp 9),TIMES (UPLUS 3,UMINUS (numexp 2))),
                 DIV (PLUS (numexp 7,TIMES (numexp 9,numexp 3)),numexp 2))),
           SET ("C",UPLUS 1),SET ("D",UMINUS (varexp "C")),
           SET
             ("A",
              PLUS (PLUS (PLUS (varexp "A",varexp "B"),varexp "C"),varexp "D")),
           ITE (GT (varexp "A",varexp "B"),SEQ [],SEQ []),
           SET
             ("A",
              PLUS (PLUS (PLUS (varexp "A",varexp "B"),varexp "C"),varexp "D")),
           SET ("B",PLUS (PLUS (varexp "B",varexp "C"),varexp "D")),
           SET ("C",PLUS (varexp "C",varexp "D")),
           SET ("D",UMINUS (varexp "D"))]); *)

(*postfix(SEQ
          [SET
             ("A",
              PLUS
                (PLUS
                   (numexp 1,
                    TIMES
                      (MINUS
                         (PLUS (TIMES (numexp 7,numexp 9),numexp 6),
                          DIV (TIMES (numexp 2,numexp 5),numexp 2)),
                       MINUS (PLUS (numexp 7,numexp 9),numexp 3))),
                 MOD (numexp 8,numexp 3))),
           SET
             ("B",
              PLUS
                (MINUS
                   (PLUS (UPLUS 8,numexp 9),TIMES (UPLUS 3,UMINUS (numexp 2))),
                 DIV (PLUS (numexp 7,TIMES (numexp 9,numexp 3)),numexp 2))),
           SET ("C",UPLUS 1),SET ("D",UMINUS (varexp "C")),
           SET
             ("A",
              PLUS (PLUS (PLUS (varexp "A",varexp "B"),varexp "C"),varexp "D")),
           SET ("E",GT (varexp "A",varexp "B")),
           SET ("E",LE (varexp "B",varexp "C")),SET ("D",UMINUS (varexp "D"))]); *)

(*
val x = top(!C);
val parse_numexp k = x;
V:=push(parse_numexp k,!V); 
C:= pop(!C);
V;
C;
val x = top(!C);
val parse_numexp k = x;
V:=push(parse_numexp k,!V); 
C:= pop(!C);
V;
C;
val a = top(!V);
val b = findValue(a);
val c = pop(!V);
val d = top(c);
val e = findValue(d);
V:= pop(!V);
V:=pop(!V);
V:= push(parse_numexp(b+e),!V);
C:= pop(!C);
V;
C;
val x = top(!C);
val parse_varexp k = x;
V:=push(parse_varexp k,!V); 
C:= pop(!C);
V;
C;
val a = top(!V);
val  parse_varexp(b) = a;
val c = pop(!V);
val d = top(c);
val e = findValue(d);
V:=pop(!V);
V:=pop(!V);
Memory.insertMem(b,e);
V;
C;
*)


(*val a = PROG("good2",SEQ(SEQ (nuexp (),
DEC (["A","B"],TYPES "int")),SEQ(SEQ (nuexp (),SET ("A",TIMES (varexp "A",varexp "B"))),SET ("B",MINUS (varexp "A",varexp "B")))));


 val PROG(b,c) = a;


fun postfix( SEQ(d)) = 




*)
