
type Motion = Up of int | Right of int | Down of int | Left of int

let lineToMagnitude (input : string) = input.Split " " |> Array.last |> int

let (|UP|_|) (input : string) = if input.StartsWith("U") then Some(lineToMagnitude input) else None
let (|DOWN|_|) (input : string) = if input.StartsWith("D") then Some(lineToMagnitude input) else None
let (|LEFT|_|) (input : string) = if input.StartsWith("L") then Some(lineToMagnitude input) else None
let (|RIGHT|_|) (input : string) = if input.StartsWith("R") then Some(lineToMagnitude input) else None

let rec solve2 (coords : (int * int) list) (motion : Motion) = 
    match motion with
    | Up 0 | Right 0 | Down 0 | Left 0 -> coords
    | Up d ->
        let x, y = coords.Head
        let coords = (x,y+1) :: coords
        solve2 coords (Up (d-1))
    | Right d ->
        let x, y = coords.Head
        let coords = (x+1,y) :: coords
        solve2 coords (Right (d-1))
    | Down d ->
        let x, y = coords.Head
        let coords = (x,y-1) :: coords
        solve2 coords (Down (d-1))
    | Left d ->
        let x, y = coords.Head
        let coords = (x-1,y) :: coords
        solve2 coords (Left (d-1))

let rec headSolver (coordinates : (int * int) list) (motions : string list) =
    match motions with
    | [] -> coordinates
    | motion :: tail -> 
        let motion =
            match motion with
            | UP i -> Up i
            | DOWN i -> Down i
            | LEFT i -> Left i
            | RIGHT i -> Right i
        let coords = solve2 [coordinates.Head] motion |> List.rev |> List.tail |> List.rev
        headSolver (coords @ coordinates) tail

let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> List.ofArray
let headCoords = headSolver [(0,0)] input |> List.rev

headCoords |> List.iter (printfn "%A")

let rec tailSolver (coords : (int * int) list) (headCoords : (int * int) list) = 
    match headCoords, coords with
    | [], _ -> coords |> List.rev
    | (hx,hy) :: t, (tx, ty) :: _ ->
        let relativePosition = hx - tx, hy - ty
        let distance = max (hx - tx |> abs) (hy - ty |> abs)
        let newPosition = 
            if distance < 2 then (tx, ty)
            else 
                match relativePosition with
                | (2,0) -> (tx+1, ty)
                | (-2,0) -> (tx-1, ty)
                | (0,2) -> (tx, ty+1)
                | (0,-2) -> (tx, ty-1)

                | (-1,2) -> (tx-1, ty+1)
                | (1,2) -> (tx+1, ty+1)

                | (2,1) -> (tx+1, ty+1)
                | (2,-1) -> (tx+1, ty-1)

                | (1,-2) -> (tx+1, ty-1)
                | (-1,-2) -> (tx-1, ty-1)

                | (-2,-1) -> (tx-1, ty-1)
                | (-2,1) -> (tx-1, ty+1)

                | (2,2) -> (tx+1, ty+1)
                | (2,-2) -> (tx+1, ty-1)
                | (-2,2) -> (tx-1, ty+1)
                | (-2,-2) -> (tx-1, ty-1)

                | _ -> failwithf "relativePosition of %A, distance of %i" relativePosition distance
        tailSolver (newPosition :: coords) t


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
