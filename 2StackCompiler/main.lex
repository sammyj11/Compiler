(*datatype lexresult= DIV | EOF | EOS | MOD |PLUS |  MINUS | TIMES | PROG| INT| BOOL| TT| FF| NOT| AND| OR| LT|LEQ|GT|GEQ|EQ|NEQ| SET | SEQ| ITE | WH | BLK | DEC |CMD | IEXP | BEXP *)
structure Tokens= Tokens
type pos = int
 type svalue = Tokens.svalue
 type ('a,'b) token = ('a,'b) Tokens.token  
 type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

  val pos = ref 0
  val lnnum = ref 1             

 val error = fn (token, col:int, lnnum:int) => TextIO.output (TextIO.stdOut,"Error at:" ^ token ^ ":"^ (Int.toString lnnum) ^ ":" ^  (Int.toString col)  ^ ", ")

 val eof = fn (file) => Tokens.EOF(!lnnum, !pos)
 
 val reserveWords = [("while", Tokens.WH), ("do", Tokens.DO), ("endwh", Tokens.ENDWH), ("if", Tokens.IF),("then", Tokens.THEN), ("else",Tokens.ELSE), ("endif",Tokens.ENDIF),("program", Tokens.PROG),("read", Tokens.READ), ("write", Tokens.WRITE),("var", Tokens.VAR), ("tt", Tokens.TT),("ff", Tokens.FF)]
 
 fun findReserveWords( token: string , lnnums : int , poss : int) =
 case List.find(fn (given, _)=> token = given) reserveWords of
   SOME (_, TOKEN) => TOKEN(lnnums, poss)
  | NONE => Tokens.VARI(token, lnnums, poss)

 
%%


%header (functor WhileLexFun(structure Tokens:While_TOKENS));
%arg(file:string);
alpha = [A-Za-z];
digit = [0-9];
 ws = [\ \t];
quote = ["];
stringstuff = [^"];
 newline = ("\r" | "\n" | "\r\n");
 
%%
 
  {newline}    =>         ( lnnum := !lnnum +1; pos :=  0 ;continue()); 
 \r\n => ( lnnum := !lnnum +1; pos :=  0 ;continue()); 
 \n => ( lnnum := !lnnum +1; pos :=  0 ;continue()); 
 \n\r => ( lnnum := !lnnum +1; pos :=  0 ;continue());  
 \r => ( lnnum := !lnnum +1; pos :=  0 ;continue());  

 {ws}+    => (continue());                               
 "/"      => (
              pos := !pos + size yytext;
              Tokens.DIV(!lnnum, !pos)
 
             );
";"      => (
              pos := !pos + size yytext;
              Tokens.EOS(!lnnum, !pos)
            );
            
"||"  => (
         
         
         pos := !pos + size yytext;
         Tokens.OR(!lnnum, !pos)
         );
         
"&&"  => (
         pos := !pos + size yytext;
         Tokens.AND(!lnnum, !pos)
         );
         
"!"  => (
         pos := !pos + size yytext;
         Tokens.NOT(!lnnum, !pos)
         );
         
">=" => (
              pos := !pos + size yytext;
              Tokens.GE(!lnnum, !pos)
        );

">"  => (
              pos := !pos + size yytext;
              Tokens.GT(!lnnum, !pos)
        );

"<=" => (
              pos := !pos + size yytext;
              Tokens.LE(!lnnum, !pos)
        );

"<"  => (
              pos := !pos + size yytext;
              Tokens.LT(!lnnum, !pos)
       );
       
":="  => (
              pos := !pos + size yytext;
              Tokens.ASSIGN(!lnnum, !pos)
       );
"<>" => (
              pos := !pos + size yytext;
              Tokens.NEQ(!lnnum, !pos)
        );
"==" => (
              pos := !pos + size yytext;
              Tokens.EQ(!lnnum, !pos)
        );

        
 "*"  => (
              pos := !pos + size yytext;
              Tokens.TIMES(!lnnum, !pos)
 
        );
"-"  => (
              pos := !pos + size yytext;
              Tokens.MINUS(!lnnum, !pos)
        );
"+"  => (
              pos := !pos + size yytext;
              Tokens.PLUS(!lnnum, !pos)
        );
 "%"  => (
              pos := !pos + size yytext;
              Tokens.MOD(!lnnum, !pos)
             
        );

 "(" => (
              pos := !pos + size yytext;
              Tokens.LPAREN(!lnnum, !pos)
        );
")" => (
              pos := !pos + size yytext;
              Tokens.RPAREN(!lnnum, !pos)
        );
"~" => (
              pos := !pos + size yytext;
              Tokens.UMINUS(!lnnum, !pos)
        );
"::" => (
              pos := !pos + size yytext;
              Tokens.DCOLON(!lnnum, !pos)
        );
":" => (
              pos := !pos + size yytext;
              Tokens.COLON(!lnnum, !pos)
        );
 "," => (
              pos := !pos + size yytext;
              Tokens.COMMA(!lnnum, !pos)
        );
 "{" => (
              pos := !pos + size yytext;
              Tokens.LBRACE(!lnnum, !pos)
        );
 "}" => (
              pos := !pos + size yytext;
              Tokens.RBRACE(!lnnum, !pos)
        );
  "bool"=>  (
              pos := !pos + size yytext;
              Tokens.BOOL(yytext,!lnnum, !pos)
        );
    "int"=>  (
              pos := !pos + size yytext;
              Tokens.INT(yytext,!lnnum, !pos)
        );
        
 {digit}+ => (
              pos := !pos + size yytext;
              Tokens.NUM
	     (List.foldl(fn (n,m) => ord(n) - ord(#"0") + 10*m) 0 (explode yytext),
	      !lnnum, !pos)
        );
    
        
   {alpha}({alpha}|{digit}| _)* => (
   pos := !pos + size yytext;
   findReserveWords(yytext, !lnnum, !pos)
        );
"."    =>(error ( yytext, !lnnum, !pos);Tokens.BADCHAR( !lnnum, !pos));
 

        
        
        
        
        
        
     




