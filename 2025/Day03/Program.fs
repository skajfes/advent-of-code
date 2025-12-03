open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (Seq.toArray >> Array.map (string>>int))

let rec max_joltage' n joltage (batteries: int[]) =
    if n = 0 then joltage else
    // printfn "%A" batteries
    let first = batteries[..^(n-1)] |> Array.max
    let idx_first = Array.findIndex ((=) first) batteries
    max_joltage' (n-1) (joltage + (int64 first) * (pown 10L (n-1))) batteries[(idx_first+1)..]

let part1 = Array.sumBy (max_joltage' 2 0L)
let part2 = Array.sumBy (max_joltage' 12 0L)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
