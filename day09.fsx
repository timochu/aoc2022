let head =
    List.rev <<
    Seq.fold (fun acc (direction, distance) ->
        List.fold (fun acc _ ->
            match direction, List.head acc with 
            | 'U', (x,y) -> (x,y+1) :: acc
            | 'D', (x,y) -> (x,y-1) :: acc
            | 'L', (x,y) -> (x-1,y) :: acc
            |  _ , (x,y) -> (x+1,y) :: acc) [acc.Head] [1 .. distance] @ acc) [0,0]

let tail = 
    List.rev <<
    List.fold (fun acc (hx, hy) -> 
        let tx, ty = List.head acc
        let dx, dy = hx-tx, hy-ty
        if max (abs(hx-tx)) (abs(hy-ty)) < 2 then acc
        else (tx+(compare dx 0), ty+(compare dy 0)) :: acc) [0,0]

let input = System.IO.File.ReadLines "inputs/day09.txt" |> Seq.map (fun s -> s[0], int s[2..])

printfn "Part 1: %i" (input |> head |> tail |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> head |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
