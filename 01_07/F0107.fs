
// 20.3.1
let vat (n: int) (x: float) :float = x + x *  float n/100.0

// 20.3.2
let unvat n x = x / (1.0 + n/100.0)

// 20.3.3
let rec helper f n = if f n = 0 then n else helper f (n + 1)
let min f = helper f 0