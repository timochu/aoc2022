System.IO.File.ReadLines "inputs/day10.txt"
|> Seq.fold(fun (acc : (int * int) list) (line:string) ->
    let cycle, register = acc |> List.head
    match line[..4] with
    | "noop" -> (cycle+1, register) :: acc
    | _ -> (cycle+2, register + int line[5..]) :: (cycle+1, register) :: acc) [1,1]
|> fun x -> [
    x |> List.find (fst >> (=) 20)
    x |> List.find (fst >> (=) 60)
    x |> List.find (fst >> (=) 100)
    x |> List.find (fst >> (=) 140)
    x |> List.find (fst >> (=) 180)
    x |> List.find (fst >> (=) 220)]
|> List.map (fun (a, b) -> a * b)
|> List.sum
|> printfn "Part 1: %A"
