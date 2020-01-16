// Learn more about F# at http://fsharp.org

// #load "IntCode.fs"
open System

let (+|) f a b = f b a

let toAscii (input:string) = 
    input
    |> (+) +| "\n"
    |> Seq.map int64
    |> Seq.toList

let fromAscii (output:int64 list) =
    output
    |> List.map (fun c -> 
        if c < 255L then (char >> string) c
        else string c)
    |> String.concat ""

[<EntryPoint>]
let main argv =
    let springbot = IO.File.ReadAllText("input.txt") |> IntCode.compile

    // part 1
    List.collect toAscii [        
        // hole 3 tiles away and footing 4 tiles away (can jump it)
        "NOT C J"
        "AND D J"
        // hole directly in front
        "NOT A T"
        "OR T J"
        "WALK"] 
    |> IntCode.run +| springbot
    |> snd 
    |> fromAscii
    |> printfn "%s"


    // part 2
    List.collect toAscii [        
        // hole 3 tiles away and footing 4 tiles away
        "NOT C J"
        "AND D J"
        // and hole not 5 or 8 tiles away
        "OR E T"
        "OR H T"
        "AND T J"

        // hole 2 tiles away
        "AND E T"
        "OR B T"
        "NOT T T"
        "OR T J"

        // hole directly in front
        "NOT A T"
        "OR T J"
        
        "RUN"] 
    |> IntCode.run +| springbot
    |> snd 
    |> fromAscii
    |> printfn "%s"
    // 1141685254

    0 // return an integer exit code

