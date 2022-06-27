structure AST = 
struct

datatype binop = PLUS | MINUS |  TIMES | DIV| MOD | AND | OR | ASSIGN | GT | GE | LE | LT | EQ | NEQ
datatype unop = NOT | UMINUS 

datatype multiexpr = ValDecls of string*exp 
and exp = numexp of int
| binexp of binop * exp * exp
| unexp of unop * exp 
| boolexp of binop * exp * exp
| stringexp of string
| varexp of string
| ITE of exp* exp * exp
| whexp of exp * exp
| consexp of string
| blkexp of string*string*exp
| seqexp of exp*exp
| strexp of exp*string
| singexp of exp
| nuexp of unit
| bexp of bool
| PROG of string*exp
| DEC of string list * exp
|SEQL of exp*exp
|CDSEQ of exp*exp
| VLIST of string list
| PLUS of exp*exp
|MINUS of exp*exp
|TIMES of exp*exp
|MOD of exp*exp
|DIV of exp*exp
|UMINUS of exp
| GT of exp*exp
| GE of exp*exp
| LT of exp*exp
| LE of exp*exp
| AND of exp*exp
| EQ of exp*exp
| OR of exp*exp
| NEQ of exp*exp
|NOT of exp
| SET of string*exp
|TYPES of string
|READ of string
|WRITE of exp
|WH of exp*exp
|UPLUS of int
|SEQ of exp list
|ENDWH of exp
and mt = parse_numexp of int
|parse_binexp of unit
|parse_unexp of unit
|parse_boolexp of unit
|parse_stringexp of unit
|parse_varexp of string
|parse_ITE of unit
|parse_whexp of unit
|parse_consexp of unit
|parse_blkexp of unit
|parse_seqexp of unit
|parse_strexp of unit
|parse_singexp of unit
|parse_nuexp of unit
|parse_bexp of bool
|parse_PROG of unit
|parse_DEC of unit
|parse_SEQL of unit
|parse_CDSEQ of unit
|parse_VLIST of unit
|parse_PLUS of unit
|parse_MINUS of unit
|parse_TIMES of unit
|parse_MOD of unit
|parse_DIV of unit
|parse_UMINUS of unit
|parse_GT of  unit
|parse_GE of unit
|parse_LT of unit
|parse_LE of unit
|parse_AND of unit
|parse_EQ of  unit
|parse_OR of  unit
|parse_NEQ of unit
|parse_NOT of unit
|parse_SET of unit
|parse_TYPES of unit
|parse_READ of unit
|parse_WRITE of unit
|parse_WH of  unit
|parse_UPLUS of int
|parse_SEQ of unit
| parse_demarker of unit
| parse_whdemark of unit



datatype oneexp = Intval of int
| boolval of bool 
| stringval of string

							    
end


















