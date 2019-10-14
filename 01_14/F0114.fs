// 40.1
let rec sum (p, xs) =
 match xs with
 | x :: xs when p x -> x + sum (p, xs)
 | x :: xs when not (p x) -> sum (p, xs)
 | _ -> 0

 // 40.2.1
let rec count (xs, n) =
 match xs with
 | y :: ys when y < n -> count (ys, n)
 | y :: ys when y = n -> 1 + count (ys, n)
 | _ -> 0

// 40.2.2
let rec insert (xs, n) =
 match xs with
 | y :: _ as ys when n <= y -> n :: ys
 | y :: ys -> y :: insert (ys, n)
 | _ -> [n]

// 40.2.3
let rec intersect (xs1, xs2) =
 match xs1, xs2 with
 | (x :: xs, y :: ys) when x = y -> x :: intersect (xs, ys)
 | (x :: xs, (y :: _ as ys)) when x < y -> intersect (xs, ys)
 | ((x :: _ as xs), y :: ys) when x > y -> intersect (xs, ys)
 | _ -> []

// 40.2.4
let rec plus (xs1, xs2) =
 match xs1, xs2 with
 | (x :: xs, (y :: _ as ys)) when x < y -> x :: plus (xs, ys)
 | ((x :: _ as xs), y :: ys) when x > y -> y :: plus (xs, ys)
 | (x :: xs, y :: ys) when x = y -> x :: y :: plus (xs, ys)
 | ([], ys) -> ys
 | (xs, []) -> xs
 | _ -> []

// 40.2.5
let rec minus (xs1, xs2) =
 match xs1, xs2 with
 | (x :: xs, (y :: _ as ys)) when x < y -> x :: minus (xs, ys)
 | (x :: xs, y :: ys) when x = y -> minus (xs, ys)
 | ((x :: _ as xs), y :: ys) when x > y -> minus (xs, ys)
 | (xs, []) -> xs
 | _ -> []

// 40.3.1
let rec smallest (xs: int list) =
 match xs with
 | [x] -> Some x
 | xs -> Some (xs |> List.min)

// 40.3.2
let rec delete (n: int, xs: int list) = // 
 match xs with
 | x :: xs when x = n -> xs
 | x :: xs -> x :: delete (n, xs)
 | _ -> []

// 40.3.3
let rec sort (xs: int list) =
 match xs with
 | _x :: _xs' ->
  let m = smallest xs
  m :: sort (delete (m.Value, xs))
 | [] -> []

// 40.4
let rec revrev = function
 | x :: xs -> revrev xs @ [List.rev x]
 | _ -> []