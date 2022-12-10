let forest = System.IO.File.ReadAllLines "inputs/day08.txt" |> Array.map (Seq.map (System.Char.ToString >> int) >> Seq.toList) |> List.ofArray

let rec resolveVisibilityAndScore (map : (bool * int) list) (trees : int list) =
    match map.Length with
    | i when i = trees.Length -> map
    | i ->
        let visibilityRight = trees[..i-1] |> List.forall ((>) trees[i])
        let visibilityLeft  = trees[i+1..] |> List.forall ((>) trees[i])
        let scoreRight      = trees[..i-1] |> List.rev |> List.tryFindIndex ((<=) trees[i]) |> function | Some x -> x + 1 | None -> i
        let scoreLeft       = trees[i+1..] |> List.tryFindIndex ((<=) trees[i]) |> function | Some x -> x + 1 | None -> trees[i+1..].Length
        resolveVisibilityAndScore (map @ [visibilityRight || visibilityLeft, scoreRight * scoreLeft]) trees

let horizontal = forest |> List.map (resolveVisibilityAndScore [])
let vertical = forest |> List.transpose |> List.map (resolveVisibilityAndScore []) |> List.transpose
let result = (horizontal, vertical) ||> List.map2 (List.map2 (fun (v1,s1) (v2,s2) -> (v1 || v2), (s1 * s2) ))

printfn "Part 1: %i" (result |> List.sumBy(List.where fst >> List.length))
printfn "Part 2: %i" (result |> List.concat |> List.maxBy snd |> snd)