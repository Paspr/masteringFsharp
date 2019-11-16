// 49.5.1
let even_seq = Seq.append (Seq.singleton 0) (Seq.initInfinite (fun i -> 2 * i))

// 49.5.2
let fac_seq = Seq.initInfinite (fun i -> [1..i] |> List.fold (*) 1)

// 49.5.3

let rec sign n = seq {
 yield -n
 yield n
 yield! sign (n+1)
}

let seq_seq = Seq.append (Seq.singleton 0) (sign 1)