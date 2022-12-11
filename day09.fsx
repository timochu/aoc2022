let head (motions : string list) =
    ([(0,0)], motions) ||> List.fold (fun acc motion ->
        let operation, distance =
            match motion.Split " " with
            | [|"U"; i|] -> (fun (x,y) -> (x,y+1)), (int i)
            | [|"D"; i|] -> (fun (x,y) -> (x,y-1)), (int i)
            | [|"L"; i|] -> (fun (x,y) -> (x-1,y)), (int i)
            | [|"R"; i|] -> (fun (x,y) -> (x+1,y)), (int i)
        let coords = ([acc.Head], [1 .. distance]) ||> List.fold (fun acc _ -> (operation acc.Head) :: acc)
        coords @ acc) |> List.rev

let tail (coordinates : (int * int) list) = 
    ([(0,0)], coordinates) ||> List.fold (fun acc (hx, hy) -> 
        let (tx, ty) = acc.Head
        let dx, dy = hx - tx, hy - ty
        let distance = max (hx - tx |> abs) (hy - ty |> abs)
        let newPosition = 
            if distance < 2 then acc.Head
            else 
                let xop = if dx > 0 then +1 elif dx < 0 then -1 else 0
                let yop = if dy > 0 then +1 elif dy < 0 then -1 else 0
                (tx+xop, ty+yop)
        newPosition :: acc
    ) |> List.rev

let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> Array.toList
printfn "Part 1: %i" (input |> head |> tail |> List.distinct |> List.length)
printfn "Part 2: %i" (input |> head |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> tail |> List.distinct |> List.length)
