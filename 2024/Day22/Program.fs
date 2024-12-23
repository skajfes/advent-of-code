open System.Diagnostics
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map int64

let next_number (num: int64) =
    let num = ((num <<< 6) ^^^ num) &&& 0xFFFFFF
    let num = ((num >>> 5) ^^^ num) &&& 0xFFFFFF
    let num = ((num <<< 11) ^^^ num) &&& 0xFFFFFF
    num

let rec nth_number n num =
    if n = 0 then num else
        nth_number (n - 1) (next_number num)

let prices num =
    [2..2000]
    |> List.mapFold (fun num _ ->
        let n = next_number num
        n, n) num
    |> fst
    |> List.append [num]
    |> List.map (fun n -> n % 10L |> int)

let part1 input =
    input
    |> Array.map (nth_number 2000)
    |> Array.sum

let find_patterns prices =
    prices
    // difference between each price
    |> List.pairwise
    |> List.map (fun (a, b) -> b, b - a)
    // patterns of 4 prices
    |> List.windowed 4
    |> List.map (fun x -> List.map snd x |> List.map string |> String.concat "" , List.last x |> fst)
    // take first price of each pattern
    |> List.groupBy fst
    |> List.map (fun (x, xs) -> x, xs |> List.map snd)
    |> List.map (fun (x, xs) -> x, xs |> List.head)
    |> List.toArray

let part2 input =
    input
    |> Array.map prices
    |> Array.map find_patterns
    |> Array.concat
    |> Array.groupBy fst
    |> Array.map (fun (x, xs) -> x, xs |> Seq.map snd |> Seq.sum)
    |> Array.maxBy snd
    |> snd

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> printfn "%A"
    // testInput |> parse |> part1 |> printfn "%A"
    // [| "123" |] |> parse |> part2 |> printfn "%A"
    // [| "1"; "2"; "3"; "2024" |] |> parse |> part2 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    let sw = Stopwatch.StartNew()
    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %A"
    printfn "Elapsed %A" sw.Elapsed

    0 // return an integer exit code
