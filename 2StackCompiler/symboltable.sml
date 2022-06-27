 
 structure SymbolTable :
 sig 
 val insert : string list*int -> unit
 val search : string ->  int option
 end =
 struct
 val TableSize = 422 (* 211 *)
 val HashFactor = 5
 val hash = fn s => List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s) 
 
 val HashTable = Array.array(TableSize,[("" , 2)]) : (string * int ) list Array.array
 
 fun insert([], _) = ()
 | insert(hd::tl, i) =
 let
 val h = hash hd
 val aplha = Array.update(HashTable,h, (hd,i) :: (Array.sub(HashTable,h))) 
 
 in
 
 insert(tl, i)
 end
 
 fun search(x)=
 let 
 val i = hash x
 fun foo((key,ans)::tl) =
 if x = key then SOME ans
 else 
 foo(tl)
 | foo(nil) = NONE
 in
 foo(Array.sub(HashTable,i))
 end
 
 end;
 
 
 


 
