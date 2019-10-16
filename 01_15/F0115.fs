// 41.4.1
let list_filter f xs =
 List.foldBack (fun param xs ->
  match f param with
  | false -> xs 
  | true -> param::xs
 )
  xs []

// 41.4.2
let sum (p, xs) = 
 List.foldBack (fun param smu ->
  match p param with
  | false -> smu
  | true -> param + smu
 )
  xs 0

// 41.4.3
let revrev lst =
 let firstReverse list = List.fold (fun head tail -> tail::head) [] list
 List.fold (fun head tail -> (firstReverse tail)::head) [] lst