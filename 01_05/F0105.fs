// 16.1
let notDivisible (m, n) = m % n = 0

// 16.2
let prime n = 
  let rec loop m = 
    if m = n/2 then true
    elif n % m = 0 then false
    else loop (m + 1)
  loop 2