open System.IO

let parse (input: string[]) = 
    input
    |> Array.map(fun line -> line.Split(' ') |> Array.map int |> Array.toList)

let is_safe report =
    report
    |> List.pairwise
    |> List.fold (fun (current_sign, valid) (a, b) ->
        let diff = abs (b - a)
        let current_sign' = sign (b - a)

        match valid with
        | false -> current_sign, false
        // check same difference
        | true when current_sign <> 0 && current_sign <> current_sign' -> current_sign', false
        // check within bounds
        | true -> current_sign', (diff >= 1 && diff <= 3)
    ) (0, true)
    |> snd

let part1 reports =
    reports
    |> Array.filter is_safe
    |> Array.length

let skip_level report =
    // get all combinations where each level is skipped for a report
    report
    |> List.mapi (fun i _ -> report[0..i-1] @ report[i+1..])

let part2 reports =
    reports
    |> Array.filter (fun r -> is_safe r || List.exists is_safe (skip_level r))
    |> Array.length


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
