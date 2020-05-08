// Learn more about F# at http://fsharp.org

open System

let mapparens =
   function
   | ')' -> -1
   | '(' -> 1
   | _ -> failwith "invalid char"
     
let part1 input =
    input
    |> Seq.map mapparens
    |> Seq.sum
    
let part2 input =
    input
    |> Seq.map mapparens
    |> Seq.indexed
    |> Seq.mapFold (fun floor (i, diff) -> (i + 1, floor + diff), floor + diff) 0
    |> fst
    |> Seq.skipWhile (snd >> (<=)0)
    |> Seq.head
    |> fst
    
[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllText("input.txt")
    part1 input |> printfn "Part1: %d"
    part2 input |> printfn "Part2: %d"
    
    0 // return an integer exit code
