let result =
    System.IO.File.ReadAllLines "inputs/day10.txt"
    |> Array.fold (fun (acc : (int * int) list) (line:string) ->
        let cycle, register = acc |> List.head
        match line[..4] with
        | "noop" -> (cycle+1, register) :: acc
        | _ -> (cycle+2, register + int line[5..]) :: (cycle+1, register) :: acc) [1,1]

result
|> List.choose (fun (c,r) -> if List.contains c [20;60;100;140;180;220] then Some(c * r) else None)
|> List.sum
|> printfn "Part 1: %A"

printfn "Part 2:"
result |> List.rev |> List.iter (fun (cycle, register) ->
    let print = if cycle % 40 = 0 then printfn else printf
    let draw = if List.contains (cycle % 40) [register;register+1;register+2] then "█" else " "
    print "%s" draw)
