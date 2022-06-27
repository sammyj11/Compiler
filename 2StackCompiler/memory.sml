 
 structure Memory :
 sig 
 val insertMem : string *int -> unit
 val searchMem : string ->  int
 val printer : unit -> unit
 val memtostr : unit -> string list
 end =
 struct
 val TableSize = 422 (* 211 *)
 val HashFactor = 5
 val hash = fn s => List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s) 
 
 val HashTable = Array.array(TableSize,("" , 0)) : (string * int )  Array.array
 

 fun insertMem(hd, i) =
 let
 	val h = hash hd
 	val aplha = Array.update(HashTable,h, (hd,i)) 
 
 in
 
 ()
 end
 
 fun searchMem(x)=
 	let 
 		val i = hash x
 		val j = Array.sub(HashTable,i)
 		val (b,c) = j
 	in
 		c
 	end
 
 
 fun printer() =
 let 
 fun prhelp(k) = 
 let 
 val(x,y) = k
 in
 if (x<> "") then print( "val " ^x ^ "->" ^ Int.toString(y) ^ "\n")
 else 
 ()
 end 
 in
 Array.app prhelp HashTable
 end
 
 fun memtostr() = 
 let 
 fun prhelp(k) = 
 let 
 val(x,y) = k
 in
 if (x<> "") then ["val " ^x ^ "->" ^ Int.toString(y)]
 else 
 []
 end
 fun valtostr(x,[]) = prhelp(x)
   | valtostr(x,l) = prhelp(x)@l; 
 in
 Array.foldr valtostr [] HashTable
 end
 end;
 
 
 


 
