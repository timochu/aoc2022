let forest = System.IO.File.ReadAllLines "inputs/day08.txt" |> Array.map (Seq.map (System.Char.ToString >> int) >> Seq.toList) |> List.ofArray

let rec checkVisibility (visibilityMap : bool list) (treeLine : int list) =
    if visibilityMap.Length = treeLine.Length then 
        visibilityMap |> List.rev
    else 
        let i = visibilityMap.Length
        let right = treeLine.[ .. i-1] |> List.forall ((>) treeLine.[i])
        let left = treeLine.[i+1..] |> List.forall ((>) treeLine.[i])
        checkVisibility ((right || left) :: visibilityMap) treeLine

let horizontal = forest |> List.map (checkVisibility [])
let vertical = forest |> List.transpose |> List.map (checkVisibility []) |> List.transpose
(horizontal, vertical) ||> List.map2 (List.map2 (||)) |> List.sumBy(List.where id >> List.length) |> printfn "%i"