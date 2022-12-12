System.IO.File.ReadLines "inputs/day10.txt"
|> Seq.fold(fun (acc : (int * int) list) (line:string) ->
    let cycle, register = acc |> List.head
    match line[..4] with
    | "noop" -> (cycle+1, register) :: acc
    | _ -> (cycle+2, register + int line[5..]) :: (cycle+1, register) :: acc) [1,1]
|> Seq.choose(fun (x,y) -> if x=20 || x=60 || x=100 || x=140 || x=180 || x=220 then Some(x*y) else None )
|> Seq.sum
|> printfn "Part 1: %A"
