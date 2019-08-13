// 20.3.1
let vat n x = x + x * (n/100.0)

// 20.3.2
let unvat n x = x / (1.0 + n/100.0)

// 20.3.3
let rec min f = if f=0 then f else min (f+1)