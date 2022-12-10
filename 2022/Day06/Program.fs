open System.IO

let parse (input: string[]) =
    input
    |> Array.head

let find x input =
    input
    |> Seq.windowed x
    |> Seq.indexed
    |> Seq.filter (fun (i, lst) -> lst |> Seq.distinct |> Seq.length = x)
    |> Seq.head
    |> fst
    |> (+) x

let part1 input = find 4 input
let part2 input = find 14 input

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
