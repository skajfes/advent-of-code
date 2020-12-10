open System.Collections.Generic
open System.IO

let parse input =
    input
    |> Seq.map int
    
let adapters input =
    let max = Seq.max input
    
    Seq.append input [max+3; 0] // add device and the plug
    |> Seq.sort

let findJolts adapters =
    adapters
    |> Seq.pairwise
    |> Seq.fold (fun (j1, j2, j3) (a1, a2) ->
        match a2 - a1 with
        | 1 -> (j1 + 1, j2, j3)
        | 2 -> (j1, j2 + 1, j3)
        | 3 -> (j1, j2, j3 + 1)
        | _ -> failwith "invalid jolt connection") (0, 0, 0)
    |> fun (j1, _, j3) -> j1 * j3

let countArrangements adapters =
    let max = Seq.max adapters
    let adapters = Seq.toList adapters
    let visited = Dictionary<int, int64>()
    
    let rec count start adapters =
        match start with
        | s when s = max -> 1L
        | s when visited.ContainsKey(s) -> visited.[s]
        | _ ->
            let matches =
                match adapters |> List.takeWhile (fun a -> a <= start + 3) with
                | [] -> 0L
                | xs -> List.fold (fun agg o -> agg + count o (List.skipWhile ((>=)o) adapters) ) 0L xs
            visited.Add(start, matches)
            matches
        
    count 0 (List.tail adapters)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    input |> parse |> adapters |> findJolts |> printfn "Part 1: %d"
    input |> parse |> adapters |> countArrangements |> printfn "Part 2: %d"
    0 // return an integer exit code