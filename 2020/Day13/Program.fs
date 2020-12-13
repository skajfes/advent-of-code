open System

let parse (input: string) =
    let arr = input.Split("\r\n")
    let ts = arr.[0] |> int64
    let buses =
        arr.[1].Split(',')
        |> Array.indexed
        |> Array.filter (snd >> (<>)"x")
        |> Array.map (fun (offset, bus) -> int64 offset, int64 bus)
    (ts, buses)
    
let earliestBus (ts, buses) =
    buses
    |> Array.map (fun (_, b) ->
        // calculate next time for bus
        let next =
            float ts / float b
            |> Math.Ceiling
            |> int64
            |> (*) b
        next - ts, b, next)
    |> Seq.sort
    |> Seq.head // take bus with lowest start time
    |> fun (d, b, _) -> d * b
    
let earliestBusChain (_, buses) =
    
    let isCorrect ts (o, b) = (ts + o) % b = 0L
    let rec findTimestamp ts =
        match buses |> Array.forall (isCorrect ts) with
        | true -> ts
        | false ->
            // increment timestamp by the multiple of all buses that match
            // general case might require the use of least common multiple alg
            let inc = buses
                      |> Array.takeWhile (isCorrect ts)
                      |> Array.fold (fun p b -> p * (snd)b) 1L
            findTimestamp (ts + inc) 
    
    findTimestamp 0L 
        
    
        
[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllText("input.txt") |> parse
    input |> earliestBus |> printfn "Part 1: %d"
    input |> earliestBusChain |> printfn "Part 2: %d"
    0 // return an integer exit code