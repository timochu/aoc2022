System.IO.File.ReadLines "inputs/day10.txt"
|> Seq.fold (fun (acc : (int * int) list) (line:string) ->
    let cycle, register = acc |> List.head
    match line[..4] with
    | "noop" -> (cycle+1, register) :: acc
    | _ -> (cycle+2, register + int line[5..]) :: (cycle+1, register) :: acc) [1,1]
|> Seq.choose (fun (c,r) -> if List.contains c [20;60;100;140;180;220] then Some(c * r) else None)
|> Seq.sum
|> printfn "Part 1: %A"
