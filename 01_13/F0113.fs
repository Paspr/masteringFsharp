// 39.1
let rec rmodd xs =
    match xs with
    | x0 :: x1 :: xs -> x1 :: rmodd xs
    | _ -> xs


// 39.2
let helper x = x % 2 = 0

let rec del_even xs =
    match xs with
    | x :: xs -> (if not(helper x) then [x] else []) @ del_even xs
    | _ -> []


// 39.3
let rec multiplicity n xs =
    match xs with
    | x :: xs ->
        let m = if n = x then 1 else 0
        m + multiplicity n xs
    | _ -> 0


// 39.4
let rec split = function
    | [] -> [], []
    | [x]-> [x], []
    | x1::x2::xs -> let xs1, xs2 = split xs
                    x1::xs1, x2::xs2


// 39.5
let rec zip (xs1, xs2) =
  match xs1, xs2 with
   | ([],[]) -> []
   | (head1::[], head2::[]) -> [(head1, head2)]
   | (head1::tail1, head2::tail2) when tail1.Length = tail2.Length -> 
       (head1, head2)::zip (tail1, tail2)
   | _ -> failwith "expected length is not equal"