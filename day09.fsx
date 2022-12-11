let rec solveStep (coords : (int * int) list) operation  magnitude = 
    if magnitude = 0 then coords
    else solveStep (coords @ [coords |> List.last |> operation]) operation (magnitude-1)

let rec headSolver (coordinates : (int * int) list) (motions : string list) =
    match motions with
    | [] -> coordinates
    | motion :: tail -> 
        let operation, magnitude =
            match motion.Split " " with
            | [|"U"; i|] -> (fun (x,y) -> (x,y+1)), (int i)
            | [|"D"; i|] -> (fun (x,y) -> (x,y-1)), (int i)
            | [|"L"; i|] -> (fun (x,y) -> (x-1,y)), (int i)
            | [|"R"; i|] -> (fun (x,y) -> (x+1,y)), (int i)
        let coords = solveStep [coordinates |> List.last] operation magnitude |> List.skip 1
        headSolver (coordinates @ coords) tail


let rec tailSolver (coords : (int * int) list) (headCoords : (int * int) list) = 
    match headCoords, coords with
    | [], _ -> coords |> List.rev
    | (hx,hy) :: t, (tx, ty) :: _ ->
        let x, y = hx - tx, hy - ty
        let distance = max (hx - tx |> abs) (hy - ty |> abs)
        let newPosition = 
            if distance < 2 then (tx, ty)
            else 
                let xop = if x > 0 then +1 elif x < 0 then -1 else 0
                let yop = if y > 0 then +1 elif y < 0 then -1 else 0
                (tx+xop, ty+yop)
        tailSolver (newPosition :: coords) t

let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> Array.toList
let headCoords = headSolver [(0,0)] input

printfn "Part 1: %i" (headCoords |> tailSolver [(0,0)] |> List.distinct |> List.length)
printfn "Part 2: %i" (
    headCoords 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> tailSolver [(0,0)] 
    |> List.distinct 
    |> List.length)
