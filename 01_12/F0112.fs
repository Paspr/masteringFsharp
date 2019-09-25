// 34.1
let rec upto (x: int) =
 match x with
 | 0 -> []
 | _ -> upto (x-1) @ [x]
 
// 34.2
let rec dnto (x: int) =
 match x with
 | 0 -> []
 | _ ->  x :: dnto (x-1) 

// 34.3
let rec evenn (x: int) =
 match x with
 | 0 -> []
 | _ -> evenn (x-1) @ [2*(x-1)]
