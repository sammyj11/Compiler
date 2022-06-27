(* nil for User declarations *)
open SymbolTable

exception typeCheckerror
%%
(*user defn*)
%name While
%arg(file ) : string
%term  EOS| OR | NOT  | AND| EOF | GE | GT | EQ | LE | LT | NEQ | ASSIGN | TT  | FF  | DIV| TIMES | UMINUS | MINUS | PLUS | MOD | WH | DO | ENDWH | IF | THEN | ELSE | ENDIF | DCOLON  | COLON | COMMA | LBRACE | RBRACE | LPAREN | RPAREN | BOOL of string | INT of string | READ | WRITE | NUM of int | VARI of string | PROG | VAR | BADCHAR

%nonterm START of AST.exp | BLK of AST.exp | DECSEQ of AST.exp | DEC of AST.exp  | VLIST of string list | TYP of AST.exp*string | COMSEQ of AST.exp | CMD of AST.exp| CMDS of AST.exp list | EXPRE of AST.exp * string 


%pos int


%eop EOF
%noshift EOF

%left OR
%left AND
%left NEQ
%left EQ 
%left GE GT LE LT
%left PLUS MINUS      (*ensure precedence by putting down, left and right assoc declarations *)
%left TIMES MOD DIV
%right UMINUS NOT


%start START

%verbose

%%


START : PROG VARI DCOLON BLK               (AST.PROG(VARI, BLK))
BLK : DECSEQ COMSEQ                 (AST.CDSEQ(DECSEQ, COMSEQ))
DECSEQ : DECSEQ DEC                  (AST.SEQL(DECSEQ,DEC))
         |                          ( AST.nuexp())
DEC : VAR VLIST COLON TYP EOS       ( if (#2(TYP) = "int") then SymbolTable.insert((VLIST,1)) else SymbolTable.insert((VLIST,0)); AST.DEC(VLIST, #1(TYP)) )
TYP : INT                            ((AST.TYPES(INT),"int"))
| BOOL                              ((AST.TYPES(BOOL),"bool"))
VLIST : VARI COMMA VLIST            ((VARI :: VLIST))
       | VARI                       ([VARI])    
COMSEQ :LBRACE CMDS RBRACE          (AST.SEQ(CMDS))
 CMDS: CMDS CMD EOS        (CMDS @[CMD])
           |                        ( [])
           
CMD    : VARI ASSIGN EXPRE (if (((SymbolTable.search(VARI) = SOME 0) andalso (#2(EXPRE) = "bool")) orelse ((SymbolTable.search(VARI) = SOME 1) andalso (#2(EXPRE) = "int"))) then AST.SET(VARI,(#1EXPRE)) else raise typeCheckerror)
         | READ VARI                (AST.READ(VARI))
         | WRITE EXPRE             (AST.WRITE((#1EXPRE)))
         | IF EXPRE THEN COMSEQ ELSE COMSEQ ENDIF    (if(#2(EXPRE) = "bool" ) then (AST.ITE((#1EXPRE),COMSEQ1,COMSEQ2)) else raise typeCheckerror)
         | WH EXPRE DO COMSEQ ENDWH (if(#2(EXPRE) = "bool" ) then (AST.WH((#1EXPRE),COMSEQ)) else raise typeCheckerror)
EXPRE : EXPRE OR EXPRE    (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.OR( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror )

|EXPRE PLUS EXPRE           ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.PLUS( (#1EXPRE1), (#1EXPRE2)),"int") else raise typeCheckerror)
| EXPRE MINUS EXPRE          ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.MINUS( (#1EXPRE1), (#1EXPRE2)),"int") else raise typeCheckerror)

|EXPRE TIMES EXPRE            ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.TIMES( (#1EXPRE1), (#1EXPRE2)),"int") else raise typeCheckerror)
| EXPRE DIV EXPRE           ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.DIV( (#1EXPRE1), (#1EXPRE2)),"int") else raise typeCheckerror)
| EXPRE MOD EXPRE           ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.MOD( (#1EXPRE1), (#1EXPRE2)),"int") else raise typeCheckerror)

|NUM                 ((AST.numexp(NUM), "int"))
|PLUS NUM                 ((AST.UPLUS(NUM), "int"))
| VARI                     (if SymbolTable.search(VARI) = SOME 0 then (AST.varexp(VARI) ,"bool" ) else (AST.varexp(VARI) ,"int"))
| LPAREN EXPRE RPAREN     ((#1(EXPRE), #2(EXPRE)))
| UMINUS EXPRE            (if #2(EXPRE) = "int" then (AST.UMINUS( (#1EXPRE)),"int") else raise typeCheckerror)

| EXPRE AND EXPRE     (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.AND( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)

|TT                      ((AST.bexp(true), "bool"))
|      FF                  ((AST.bexp(false), "bool"))

| NOT EXPRE             (if #2(EXPRE) = "bool" then (AST.NOT( (#1EXPRE)),"bool") else raise typeCheckerror)

|EXPRE GT EXPRE        (if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.GT( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)
|EXPRE GE EXPRE        (if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.GE( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)
|EXPRE LE EXPRE       (if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.LE( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)
|EXPRE LT EXPRE        (if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.LT( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)
|EXPRE EQ EXPRE       (if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") )then (AST.EQ( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)
|EXPRE NEQ EXPRE        (if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") )then (AST.NEQ( (#1EXPRE1), (#1EXPRE2)),"bool") else raise typeCheckerror)









(*version 2 isme bas exre replace hai har jagaha bahut saare conflicts are*)
