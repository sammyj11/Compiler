structure FunStack :
sig
type 'a Stack
exception EmptyStack
exception Error of string
val create: 'a Stack
val push : 'a * 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a Stack
val top : 'a Stack -> 'a
val empty: 'a Stack -> bool
val poptop : 'a Stack -> ('a * 'a Stack) option
val nth : 'a Stack * int -> 'a
val drop : 'a Stack * int -> 'a Stack
val depth : 'a Stack -> int
val app : ('a -> unit) -> 'a Stack -> unit
val map : ('a -> 'b) -> 'a Stack -> 'b Stack
val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
val find : ('a -> bool) -> 'a Stack -> 'a option
val filter : ('a -> bool) -> 'a Stack -> 'a Stack
val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val exists : ('a -> bool) -> 'a Stack -> bool
val all : ('a -> bool) -> 'a Stack -> bool
val list2Stack : 'a list -> 'a Stack (* Convert a list into a Stack *)
val Stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)

end =
struct
  
  type 'a Stack = 'a list

exception EmptyStack
exception Error of string
(* functions to add: toString*)

val create : 'a Stack = []
  
fun push(elem , []) = [elem]
| push(elem , stk) = elem :: stk

fun pop([]) = raise EmptyStack
| pop(hd::tl) = tl

fun top([]) = raise EmptyStack
| top(hd::tl) = hd

fun empty([]) = true
| empty(hd::tl) = false

fun poptop([]) = raise EmptyStack
| poptop(hd::tl) = SOME (hd, tl)

fun nth(stk, i) = List.nth(stk ,i)

fun drop(stk, i) = List.drop(stk, i)

fun depth([]) = 0
| depth(hd::tl) = 1+ depth(tl)

fun app f stk = List.app f stk

fun map f stk = List.map f stk

fun mapPartial f stk = List.mapPartial f stk

fun find f stk = List.find f stk

fun filter f stk = List.filter f stk

val foldl = List.foldl

val foldr = List.foldr

fun exists f stk = List.exists f stk

fun all f stk = List.all f stk

fun Stack2list (a : 'a Stack): 'a list= a

fun list2Stack (a : 'a list): 'a Stack= a

end;






