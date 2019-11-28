// 50.2.1
let fac_seq = seq {
    yield 1
    yield! Seq.initInfinite (fun i -> [1..i] |> List.fold (*) 1) |> Seq.skip 1
}

// 50.2.2
let seq_seq = seq {
    let rec sign n = seq {
            yield -n
            yield n
            yield! sign (n+1)
    }
    yield 0 
    yield! sign 1
}