let head =
    Seq.fold (fun acc (direction, distance) ->
        let folder acc _ =
            match direction, List.head acc with 
            | 'U', (x,y) -> (x,y+1) :: acc
            | 'D', (x,y) -> (x,y-1) :: acc
            | 'L', (x,y) -> (x-1,y) :: acc
            |  _ , (x,y) -> (x+1,y) :: acc
        List.fold folder [acc.Head] [1 .. distance] @ acc) [0,0] >> List.rev

let tail = 
    List.fold (fun acc (hx, hy) -> 
        let tx, ty = List.head acc
        let dx, dy = hx - tx, hy - ty
        if max (abs(hx-tx)) (abs(hy-ty)) < 2 then acc
        else (tx + (compare dx 0), ty + (compare dy 0)) :: acc) [0,0] >> List.rev

let input = System.IO.File.ReadLines "inputs/day09.txt" |> Seq.map (fun s -> s[0], int s[2..])

printfn "Part 1: %i" (input |> head |> tail |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> head |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
