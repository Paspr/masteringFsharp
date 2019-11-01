// 47.4.1
let f n = 
 if (n=0 || n=1) then 1
 else
    let mutable x = 1
    let mutable counter = n
    while counter > 0 do
     x <- x * counter
     counter <- counter - 1
    x


// 47.4.2
let fibo n =
 if n=0 then 0
 else
 if n=1 then 0
 else
   let mutable x = 1
   let mutable y = 0
   let mutable counter = n
   while counter >= 2 do
    let x1 = x + y
    y <- x
    x <- x1
    counter <- counter - 1
   x