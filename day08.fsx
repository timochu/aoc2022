let forest = System.IO.File.ReadAllLines "inputs/day08.txt" |> Array.map (Seq.map (System.Char.ToString >> int) >> Seq.toList) |> List.ofArray

let rec calculateScore (score : int) (tree : int) (line : int list) =
    match line with
    | [] -> score
    | h :: _ when tree = h || tree < h -> score + 1
    | _ :: t -> calculateScore (score + 1) tree t

let rec checkVisiblityAndScore (map : (bool * int) list) (trees : int list) =
    match map.Length with
    | i when i = trees.Length -> map
    | i ->
        let visibilityRight = trees.[..i-1] |> List.forall ((>) trees.[i])
        let visibilityLeft = trees.[i+1..] |> List.forall ((>) trees.[i])
        let scoreRight = trees.[..i-1] |> List.rev |> calculateScore 0 trees.[i]
        let scoreLeft = trees.[i+1..] |> calculateScore 0 trees.[i]
        checkVisiblityAndScore (map @ [visibilityRight || visibilityLeft, scoreRight * scoreLeft]) trees

let horizontal = forest |> List.map (checkVisiblityAndScore [])
let vertical = forest |> List.transpose |> List.map (checkVisiblityAndScore []) |> List.transpose
let result = (horizontal, vertical) ||> List.map2 (List.map2 (fun (v1,s1) (v2,s2) -> (v1 || v2), (s1 * s2) ))

printfn "Part 1: %i" (result |> List.sumBy(List.where fst >> List.length))
printfn "Part 2: %A" (result |> List.concat |> List.maxBy snd)