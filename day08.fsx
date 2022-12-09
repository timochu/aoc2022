let forest = System.IO.File.ReadAllLines "inputs/day08.txt" |> Array.map (Seq.map (System.Char.ToString >> int) >> Seq.toList) |> List.ofArray

let rec checkVisibility (visibilityMap : bool list) (treeLine : int list) = 
    if visibilityMap.Length = treeLine.Length then 
        visibilityMap |> List.rev
    else 
        let i = visibilityMap.Length
        let right = treeLine.[ .. i-1] |> List.forall (fun x -> x < treeLine.[i])
        let left = treeLine.[i+1..] |> List.forall (fun x -> x < treeLine.[i])
        checkVisibility ((right || left) :: visibilityMap) treeLine

let result = forest |> List.map (checkVisibility [])
let result2 = forest |> List.transpose |> List.map (checkVisibility []) |> List.transpose
let result3 = (result, result2) ||> List.map2 (fun horizontal vertical -> (horizontal, vertical) ||> List.map2 (fun h v -> h || v))

result3 |> List.sumBy(fun row -> row |> List.where (fun y -> y = true) |> List.length) |> printfn "%A"