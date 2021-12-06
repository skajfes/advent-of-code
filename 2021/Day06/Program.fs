open System.IO

let parse (input: string) = 
    input.Split(',')
    |> Array.map int
    |> List.ofArray
    |> List.countBy id
    |> List.map (fun (fish, count) -> fish, int64 count)

let one_day fish =
    ([], fish)
    ||> List.fold (fun fish_out (fish, count) ->
        match fish with
        | 0 -> (6, count)::(8, count)::fish_out
        | x -> (x - 1, count)::fish_out)
    |> List.groupBy fst
    |> List.map (fun (fish, fishes) -> fish, fishes |> List.sumBy snd)
    // |> fun f ->
    //     printfn "%A" f
    //     f

let simulate_days days fish =
    (fish, [1..days])
    ||> List.fold (fun fish _ -> one_day fish)
    |> List.sumBy snd

let part1 fish =
    simulate_days 80 fish

let part2 fish =
    simulate_days 256 fish

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = "3,4,3,1,2"
    
    testInput |> parse |> part1 |> printfn "%d"
    
    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
