open System.IO

let parse (input: string[]) =
    input
    |> Array.map int

let part1 (measurements: int[]) =
    measurements
    |> Array.pairwise
    |> Array.where (fun (a, b) -> b > a)
    |> Array.length

let part2 (measurements: int[]) =
    measurements
    |> Array.windowed 3
    |> Array.map (Array.sum)
    |> Array.pairwise
    |> Array.where (fun (a, b) -> b > a)
    |> Array.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
