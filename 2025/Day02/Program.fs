
open System.IO

let parse (input: string[]) =
    input
    |> Array.head
    |> _.Split([|','|])
    |> Array.map (_.Split([|'-'|]) >> fun [| a; b |] -> int64 a, int64 b)

// get multiplier of n, its 10 with number of zeroes as length 72 -> 100
let digits n =
    string n |> String.length |> pown 10L

// construct a number by repeating digits x n times, multiplying by d everytime
let rec construct (x: int64) d n =
    match n with
    | 1 -> x
    | _ -> (construct x d (n - 1)) * d + x

// generate all invalid numbers where pattern repeats n times, in range start-stop
let valid_in_range n start stop =
    let len = string start |> String.length
    let len' = len / n
    let multiplier = pown 10L (len - len')

    let start' = start / multiplier
    let stop' = stop / multiplier

    // printfn "%d %d %d %d" a d start stop

    [|for i in start'..stop' -> construct i (digits i) n |]
    |> Array.filter (fun x -> x >= start && x <= stop)

// generate all invalid numbers where pattern repeats 2 to length of stop times
let valid_in_ranges start stop =
    let len = string stop |> String.length
    [|2..len|]
    |> Array.map (fun n -> valid_in_range n start stop)
    |> Array.concat
    |> Array.distinct
    |> Array.sum

let part1 ranges =
    ranges
    |> Array.map (fun (a, b) -> valid_in_range 2 a b)
    |> Array.concat
    |> Array.sum

let part2 ranges =
    ranges
    |> Array.map (fun (a, b) -> valid_in_ranges a b)
    |> Array.sum


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
