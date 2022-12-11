let head (motions : string array) =
    ([(0,0)], motions) ||> Array.fold (fun acc motion ->
        let operation, distance =
            match motion.Split " " with
            | [|"U"; i|] -> (fun (x,y) -> (x,y+1)), int i
            | [|"D"; i|] -> (fun (x,y) -> (x,y-1)), int i
            | [|"L"; i|] -> (fun (x,y) -> (x-1,y)), int i
            | [|"R"; i|] -> (fun (x,y) -> (x+1,y)), int i
        let coords = ([acc.Head], [1 .. distance]) ||> List.fold (fun acc _ -> (operation acc.Head) :: acc)
        coords @ acc) |> List.rev

let tail (coordinates : (int * int) list) = 
    ([(0,0)], coordinates) ||> List.fold (fun acc (hx, hy) -> 
        let tx, ty = acc.Head
        let dx, dy = hx - tx, hy - ty
        if max (hx - tx |> abs) (hy - ty |> abs) < 2 then acc.Head :: acc
        else (tx + (compare dx 0), ty + (compare dy 0)) :: acc) |> List.rev

let input = System.IO.File.ReadAllLines "inputs/day09.txt"
printfn "Part 1: %i" (input |> head |> tail |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> head |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
