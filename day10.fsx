let result =
    System.IO.File.ReadAllLines "inputs/day10.txt"
    |> Array.fold (fun acc line ->
        let c, r = List.head acc
        match line[..4] with
        | "noop" -> (c+1, r) :: acc
        | _ -> (c+2, r + int line[5..]) :: (c+1, r) :: acc) [1,1] |> List.rev

printfn "Part 1: %i" (result |> List.sumBy (fun (c,r) -> if List.contains c [20;60;100;140;180;220] then c*r else 0))

printfn "Part 2:"
result |> List.iter (fun (c, r) -> (if List.contains (c % 40) [r..r+2] then "█" else " ") |> printf (if c % 40 = 0 then "%s\n" else "%s"))
