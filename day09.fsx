#time
let head =
    Seq.fold (fun acc (direction, distance) ->
        List.fold (fun acc i ->
            let x, y = List.last acc
            match direction with | 'U' -> x,y+i | 'D' -> x,y-i | 'L' -> x-i,y | _ -> x+i,y
            :: acc) [acc[0]] [1 .. distance] @ acc) [0,0] >> List.rev

let tail = 
    List.fold (fun acc (hx, hy) -> 
        let tx, ty = List.head acc
        let dx, dy = hx-tx, hy-ty
        if max(abs(dx)) (abs(dy)) < 2 then acc
        else (tx+(compare dx 0), ty+(compare dy 0)) :: acc) [0,0] >> List.rev

let input = System.IO.File.ReadLines "inputs/day09.txt" |> Seq.map (fun s -> s[0], int s[2..]) |> head |> tail

printfn "Part 1: %i" (input |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
