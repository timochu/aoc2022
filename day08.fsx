let forest = System.IO.File.ReadAllLines "inputs/day08.txt" |> Array.map (Seq.map (System.Char.ToString >> int) >> Seq.toList) |> List.ofArray

let rec checkVisibility (visibilityMap : (bool * int) list) (treeLine : int list) =
    if visibilityMap.Length = treeLine.Length then 
        visibilityMap |> List.rev
    else 
        let i = visibilityMap.Length
        let right = treeLine.[ .. i-1] |> List.forall ((>) treeLine.[i])
        let left = treeLine.[i+1..] |> List.forall ((>) treeLine.[i])
        let rec getScore (score : int) (tree : int) (line : int list) =
            match line with
            | [] -> score
            | h :: _ when tree = h || tree < h -> score + 1
            | _ :: t -> getScore (score + 1) tree t

        let rightScore = treeLine.[..i-1] |> List.rev |> getScore 0 treeLine.[i]
        let leftScore = treeLine.[i+1..] |> getScore 0 treeLine.[i]

        checkVisibility ((right || left, rightScore * leftScore) :: visibilityMap) treeLine

let horizontal = forest |> List.map (checkVisibility [])
let vertical = forest |> List.transpose |> List.map (checkVisibility []) |> List.transpose
(horizontal, vertical) ||> List.map2 (List.map2 (fun (x,y) (a,b) -> (x || a), (y * b) )) |> List.sumBy(List.where (fst >> id) >> List.length) |> printfn "Part 1: %i"
(horizontal, vertical) ||> List.map2 (List.map2 (fun (x,y) (a,b) -> (x || a), (y * b) )) |> List.concat |> List.map snd |> List.max |> printfn "Part 2: %i"
