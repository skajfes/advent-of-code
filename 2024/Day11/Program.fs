open System.IO

let parse (input: string[]) = 
    input
    |> Array.head
    |> _.Split(' ')
    |> Array.map int64
    |> Array.toList
    |> List.map (fun x -> x, 1L)

let count_stones =
    List.groupBy fst
    >> List.map (fun (s, ss) -> s, ss |> List.sumBy snd)

let rec blink n stones =
    if n = 0 then stones else

    stones
    |> List.fold (fun res (s, count) ->
        match s with
        | 0L -> (1L, count)::res
        | x when (x|>string|>String.length) % 2 = 0 ->
            let ss = string s
            let len = ss.Length/2
            let fst = ss.Substring(0, len)
            let snd = ss.Substring(len)
            (int snd, count)::(int fst, count)::res
        | x -> (x * 2024L, count)::res
        ) []
    |> count_stones
    |> blink (n-1)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> blink 25 |> List.sumBy snd |> printfn "%A"

    input |> parse |> blink 25 |> List.sumBy snd |> printfn "Part 1: %d"
    input |> parse |> blink 75 |> List.sumBy snd |> printfn "Part 2: %d"

    0 // return an integer exit code
