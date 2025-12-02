
open System.IO

let parse (input: string[]) =
    input
    |> Array.head
    |> _.Split([|','|])
    |> Array.map (_.Split([|'-'|]) >> fun [| a; b |] -> int64 a, int64 b)


let is_valid s =
    let digits = string s |> String.length
    let digits2 = digits / 2
    let d = pown 10L (digits2)

    let a = s / d
    let b = s % d

    printfn "%d %d %d %d %A" s digits digits2 d (a=b)

    a = b

let digits n =
    string n |> String.length |> pown 10L

let multiplier n =
    let digits = string n |> String.length
    let digits2 = digits / 2
    let rest = digits - digits2
    pown 10L (rest)

let valid_in_range a b =
    let d = multiplier a

    let start = a / d
    let stop = b / d

    // printfn "%d %d %d %d" a d start stop

    [|for i in start..stop -> i*(digits i)+i|]
    |> Array.filter (fun x -> x >= a && x <= b)

let valid_in_ranges a b =
    valid_in_range

let part1 ranges =
    ranges
    |> Array.map (fun (a, b) -> valid_in_range a b)
    |> Array.concat
    |> Array.sum


let part2 ranges =
    ranges
    |> Array.map (fun (a, b) -> valid_in_ranges a b)




[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    // input |> parse |> part1 |> printfn "Part 1: %d"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
