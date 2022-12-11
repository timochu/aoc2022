let headSolver (motions : string list) =
    ([(0,0)], motions) ||> List.fold (fun coordinates motion ->
        let operation, distance =
            match motion.Split " " with
            | [|"U"; i|] -> (fun (x,y) -> (x,y+1)), (int i)
            | [|"D"; i|] -> (fun (x,y) -> (x,y-1)), (int i)
            | [|"L"; i|] -> (fun (x,y) -> (x-1,y)), (int i)
            | [|"R"; i|] -> (fun (x,y) -> (x+1,y)), (int i)
        let coords = ([coordinates.Head], [1 .. distance]) ||> List.fold (fun acc _ -> (acc.Head |> operation) :: acc) |> List.rev |> List.tail |> List.rev
        coords @ coordinates) |> List.rev

let rec tailSolver (headCoords : (int * int) list) = 
    ([(0,0)], headCoords) ||> List.fold (fun coords (hx, hy) -> 
        let (tx, ty) = coords |> List.head
        let x, y = hx - tx, hy - ty
        let distance = max (hx - tx |> abs) (hy - ty |> abs)
        let newPosition = 
            if distance < 2 then (tx, ty)
            else 
                let xop = if x > 0 then +1 elif x < 0 then -1 else 0
                let yop = if y > 0 then +1 elif y < 0 then -1 else 0
                (tx+xop, ty+yop)
        newPosition :: coords
    ) |> List.rev

let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> Array.toList

printfn "Part 1: %i" (input |> headSolver |> tailSolver |> List.distinct |> List.length)
printfn "Part 2: %i" (
    input 
    |> headSolver 
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> tailSolver
    |> List.distinct 
    |> List.length)
