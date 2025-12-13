open System.IO
open System
open System.Collections.Generic

let parse (input: string[]) = 
    input
    |> Array.map (fun s -> s.Split(':', StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun [|a;bs|] -> a, bs.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
    |> Map.ofArray

let visit map start stop =
    let visited = Dictionary<string, int>()
    let rec walk node end_node path =
        //printfn "%A" node
        if node = end_node then 
            //printfn "found: %A" path
            1 else
        if node = "out" then 0 else
        if visited.ContainsKey node then visited[node] else

        let sum = 
            Map.find node map
            |> List.map (fun n -> walk n end_node (n::path))
            |> List.sum

        visited.Add( node,  sum) |> ignore
        sum

    walk start stop [start]

let part1 map = visit map "you" "out"

let part2 map =
    
    let start_fft = visit map "svr" "fft"
    let fft_dac = visit map "fft" "dac" 
    let dac_out = visit map "dac" "out"

    int64 fft_dac * int64 start_fft * int64 dac_out


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    let testInput2 = File.ReadAllLines("sample2.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput2 |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
