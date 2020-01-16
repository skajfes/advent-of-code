// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =

    let calc op arg1 arg2 =
        match op with
        | 1 -> arg1 + arg2 
        | 2 -> arg1 * arg2 
        | _ -> failwith (sprintf "unknown opcode: %d" op)

    let parse (input: int[]) pos =
        let op = input.[pos]
        
        if (op = 99) then true 
        else 
            let arg1 = input.[input.[pos + 1]]
            let arg2 = input.[input.[pos + 2]]
            let resPtr = input.[pos + 3]
            let res = calc op arg1 arg2
            input.[resPtr] <- res
            false

    let run noun verb =
        let input = 
            File.ReadAllText ("input.txt") 
            |> (fun x -> x.Split(','))
            |> Array.map int

        input.[1] <- noun
        input.[2] <- verb

        let mutable counter = 0
        
        let mutable finished = false
        while not finished do
            finished <- parse input counter
            counter <- counter + 4

        input.[0]

    [for noun in [0..99] do for verb in [0..99] -> (noun, verb)]
    |> Seq.map (fun (noun, verb) -> (noun, verb, run noun verb))
    |> Seq.filter (fun (_, _, r) -> r = 19690720)
    |> Seq.item 0
    |> (fun (noun, verb, _) -> 100 * noun + verb)
    