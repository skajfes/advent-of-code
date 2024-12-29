open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.fold (fun (towels, patterns) line ->
        if line.Contains(',') then
            line.Split(", "), []
        elif line.Length = 0 then towels, patterns
        else towels, line::patterns
    ) ([||], [])

let mem = Dictionary<string, int64>()
let rec create_pattern (towels: string array) (pattern: string) =
    match mem.TryGetValue pattern with
    | true, v -> v
    | false, _ ->
        if pattern = "" then 1L else
        towels
        |> Array.filter pattern.StartsWith
        |> Array.sumBy (fun t -> create_pattern towels (pattern.Substring(t.Length)))
        |> fun res ->
            mem.Add(pattern, res)
            res

let part1 (towels, patterns) =
    patterns
    |> List.filter (create_pattern towels >> (<)0L)
    |> List.length

let part2 (towels, patterns) =
    patterns
    |> List.sumBy (create_pattern towels)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"
    mem.Clear() // clear cache between test and real cases

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
