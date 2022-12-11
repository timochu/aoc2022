let head motions =
    ([(0,0)], motions) ||> Array.fold (fun acc (direction, distance) ->
        let operation =
            match direction with
            | 'U' -> fun (x,y) -> x, y + 1
            | 'D' -> fun (x,y) -> x, y - 1
            | 'L' -> fun (x,y) -> x - 1, y
            | _   -> fun (x,y) -> x + 1, y
        (([acc.Head], [1 .. distance]) ||> List.fold (fun acc _ -> (operation acc.Head) :: acc)) @ acc) 
    |> List.rev

let tail coordinates = 
    ([(0,0)], coordinates) ||> List.fold (fun acc (hx, hy) -> 
        let tx, ty = acc.Head
        let dx, dy = hx - tx, hy - ty
        if max (hx - tx |> abs) (hy - ty |> abs) < 2 then acc.Head :: acc
        else (tx + (compare dx 0), ty + (compare dy 0)) :: acc) |> List.rev

let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> Array.map (fun (s) -> Seq.head s, s.Substring 2 |> int)
printfn "Part 1: %i" (input |> head |> tail |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> head |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
