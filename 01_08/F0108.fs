// 21
let curry f (x: int) (y :int) = f (x, y) :int

let uncurry f = fun (x, y) -> f (x :int) (y :int) :int
