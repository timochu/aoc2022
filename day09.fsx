let head motions =
    ([0,0], motions) ||> Seq.fold (fun acc (direction, distance) ->
        let folder (acc : list<int * int>) _ =
            match direction, acc.Head with
            | 'U', (x,y) -> (x, y + 1) :: acc
            | 'D', (x,y) -> (x, y - 1) :: acc
            | 'L', (x,y) -> (x - 1, y) :: acc
            | _  , (x,y) -> (x + 1, y) :: acc
        List.fold folder [acc.Head] [1 .. distance] @ acc)
    |> List.rev

let tail coordinates = 
    ([0,0], coordinates) ||> List.fold (fun acc (hx, hy) -> 
        let tx, ty = acc.Head
        let dx, dy = hx - tx, hy - ty
        if max (hx - tx |> abs) (hy - ty |> abs) < 2 then acc.Head :: acc
        else (tx + (compare dx 0), ty + (compare dy 0)) :: acc) |> List.rev

let input = System.IO.File.ReadLines "inputs/day09.txt" |> Seq.map (fun s -> s[0], int s[2..])

printfn "Part 1: %i" (input |> head |> tail |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> head |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
