open System.IO

let parse (input: string) =
    input.Split("\n\n")
    |> Array.map (_.Split("\n"))
    |> Array.fold (fun (locks, keys) schema ->
        let t = schema |> Seq.transpose |> Seq.map (Seq.filter ((=) '#') >> Seq.length >> (+) -1) |> Seq.toArray
        if schema[0] = "#####" then
            t::locks, keys
        else
            locks, t::keys
    ) ([], [])

let fit_locks (locks, keys) =
    locks
    |> List.collect (fun lock ->
        keys
        |> List.map (fun key -> lock, key)
        )
    |> List.filter (fun (lock, key) ->
        Array.zip lock key
        |> Array.forall (fun (lock, key) -> lock + key <= 5))
    |> List.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample.txt")
    
    testInput |> parse |> fit_locks |> printfn "%A"
    
    input |> parse |> fit_locks |> printfn "Part 1: %A"

    0 // return an integer exit code
