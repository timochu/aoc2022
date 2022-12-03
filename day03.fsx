// puzzle: https://adventofcode.com/2022/day/3
#time

let priority = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

"inputs/day03.txt" 
|> System.IO.File.ReadAllLines
|> Array.sumBy(
        Seq.map (fun c -> priority |> Seq.findIndex (fun i -> i = c))
        >> Seq.splitInto 2
        >> Seq.map set
        >> (fun x -> Set.intersect (Seq.head x) (Seq.last x))
        >> Set.maxElement)
|> printfn "%i"
