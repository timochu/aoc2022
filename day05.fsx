// puzzle: https://adventofcode.com/2022/day/4
#time

open System
open System.Collections.Generic
open type System.StringSplitOptions

let stacksInput, ordersInput = 
    IO.File.ReadAllLines "inputs/day05.txt"
    |> fun i -> Array.take 8 i, Array.skip 10 i

let stacks = 
    [| 1 .. +4 .. 33 |]
    |> Array.map (fun column ->
        [| 7 .. -1 .. 0 |]
        |> Seq.map (fun row ->
            stacksInput.[row].[column])
            |> Seq.where ((=) ' ' >> not)
            |> Stack)

let orders =
    ordersInput
    |> Array.map (
        fun x -> x.Split([|"move "; " from "; " to "|], RemoveEmptyEntries)
        >> Array.map int)

orders
|> Array.iter (fun x ->
    [|1 .. x.[0]|]
    |> Array.iter (fun _ ->
        let crate = stacks.[x.[1]-1].Pop()
        stacks.[x.[2]-1].Push(crate)
    )
)

stacks 
|> Array.map (fun (x : Stack<char>) -> x.Peek())
|> String
|> printfn "Part 1: %s"