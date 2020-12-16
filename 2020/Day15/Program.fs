open System.Collections.Generic

let split (input: string) =
    input.Split(',')
    |> Array.map int64
    
let spokenNumbers at_turn input =
    let spoken = Dictionary<int64, int64>()
    
    // fill the dictionary
    Array.mapi (fun i n -> spoken.Add(n, int64 i + 1L)) input |> ignore
    
    let rec numbers turn current =
        if turn = at_turn then current else
            
        let next = 
            match spoken.TryGetValue(current) with
            | false, _ -> 0L
            | true, n -> turn - n
            
        spoken.[current] <- turn
        numbers (turn + 1L) next
    
    numbers (int64 (spoken.Count)) (Seq.last input) 

[<EntryPoint>]
let main argv =
    "0,20,7,16,1,18,15" |> split |> spokenNumbers 2020L |> printfn "Part 1: %d"
    "0,20,7,16,1,18,15" |> split |> spokenNumbers 30000000L |> printfn "Part 2: %d"
    0 // return an integer exit code