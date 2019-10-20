// 42.3
let rec allSubsets n k =
    match n, k with
    | _, 0 -> Set.empty.Add(Set.empty)
    | 0, _ -> Set.empty
    | n, k -> Set.union (Set.map (fun s -> Set.add n s) (allSubsets (n-1) (k-1)))
                (allSubsets (n-1) k)