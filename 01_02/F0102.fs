// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 

    let g n = n + 5 // int

    let gg = fun n -> n+5

    let h (x,y) = System.Math.Sqrt(x*x+y*y)

    0 // return an integer exit code
