open System
open System.IO

let parse (input: string[]) = 
    input
    |> Array.take 2
    |> Array.map (fun x -> x.Substring(x.IndexOf(':') + 1))
    |> Array.map (fun x -> x.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64)
    |> Array.transpose
    |> Array.map (fun [| t; d |] -> t, d)

let parse' (input: string[]) =
    input
    |> Array.take 2
    |> Array.map (fun x -> x.Substring(x.IndexOf(':') + 1))
    |> Array.map (fun x -> x.Replace(" ", "") |> int64)
    |> function | [| t; d |] -> (t, d)


let toWin (time, distance) =
    [1L..time-1L]
    |> List.map (fun t -> (time - t) * t)
    |> List.filter (fun d -> d > distance)
    |> List.length

let part1 times =
    times
    |> Array.map toWin
    |> Array.fold (fun a b -> a * b) 1

let part2brute times =
    times
    |> Array.map toWin
    |> Array.fold (fun a b -> a * b) 1

let part2 (time, distance) =
    // quadratic equation
    // t = (T +- sqrt(T2 - 4d))/2
    let t = float time
    let d = float distance
    let t1 = (t + sqrt (t*t - 4.0 * d))/2.0 |> floor |> int
    let t2 = (t - sqrt (t*t - 4.0 * d))/2.0 |> ceil |> int
    t1 - t2 + 1

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse' |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse' |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
