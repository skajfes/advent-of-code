open System
open System.IO

let parse (input: int[]) pos =

    let op = input.[pos] 
    let argMode offset = 
        op / 100 / pown 10 (offset - 1) % pown 10 (offset)

    let read offset =
        match argMode offset with
        | 0 -> input.[input.[pos + offset]]
        | 1 -> input.[pos + offset]
        | _ -> failwith "invalid arg mode"

    let write offset r =
        let resPtr = input.[pos + offset]
        input.[resPtr] <- r

    let command f =
        f (read 1) (read 2) |> write 3
        pos + 4

    let jumpCommand f =
        if f (read 1) then read 2 else pos + 3

    let input() =
        Console.ReadLine() |> int |> write 1
        pos + 2

    let output() =
        read 1 |> printfn "%i" 
        pos + 2


    match op % 100 with
    | 1 -> command (+)
    | 2 -> command (*)
    | 3 -> input()
    | 4 -> output()
    | 5 -> jumpCommand ((<>) 0)
    | 6 -> jumpCommand ((=) 0)
    | 7 -> command (fun a b -> if a < b then 1 else 0)
    | 8 -> command (fun a b -> if a = b then 1 else 0)
    | 99 -> -1
    | _ -> failwith (sprintf "unknown opcode: %d" op)

let run input =

    let mutable counter = 0
    
    while counter <> -1 do
        counter <- parse input counter

    input.[0]


[<EntryPoint>]
let main argv =
    let input = 
        File.ReadAllText ("input.txt") 
        |> (fun x -> x.Split(','))
        |> Array.map int

    run input
    
