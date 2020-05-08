// Learn more about F# at http://fsharp.org

open System

let parseInput (input: string []) =
    input
    |> Seq.map (fun t -> t.Split('x'))
    |> Seq.map (function
        | [| a; b; c |] -> (int a, int b, int c)
        | _ -> failwith "invalid format")
    
let paperForPresent (a, b, c) =
    let surfaces = List.sort [ a * b; a * c; b * c ]
    surfaces.[0] + 2 * List.sum surfaces
    
let ribbonForPresent (a, b, c) =
    let lengths = List.sort [ 2 * (a + b); 2 * (a + c); 2 * (b + c) ]
    a * b * c + lengths.[0]


[<EntryPoint>]
let main argv =
    let presents = IO.File.ReadAllLines("input.txt") |> parseInput
    //    paperForPresent (2, 3, 4) |> printfn "%d"
    //    paperForPresent (1, 1, 10) |> printfn "%d"
    //    ribbonForPresent (2, 3, 4) |> printfn "%d"
    //    ribbonForPresent (1, 1, 10) |> printfn "%d"
    
    Seq.sumBy paperForPresent presents |> printfn "Part1 - total paper: %d"
    Seq.sumBy ribbonForPresent presents |> printfn "Part2 - total ribbon: %d"

    0 // return an integer exit code
