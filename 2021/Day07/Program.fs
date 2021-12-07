open System.IO

let parse (input: string) = 
    input.Split(',')
    |> Array.map int

let part1 crabs =
    let medianPosition =
        crabs
        |> Array.sort
        |> Array.skip (Array.length crabs / 2)
        |> Array.head

    crabs
    |> Array.sumBy (fun c -> c - medianPosition |> abs)


let part2 crabs =
    let averagePosition =
        crabs
        |> Array.averageBy decimal
        |> round
        |> int

    [averagePosition - 1 .. averagePosition + 1]
    |> List.map (fun average ->
        crabs
        |> Array.map (fun c -> c - average |> abs)
        |> Array.sumBy (fun c -> [1..c]|>List.sum))
    |> List.min

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = "16,1,2,0,4,2,7,1,2,14"
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
