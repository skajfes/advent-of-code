open System.IO

let parse (input: string[]) =
    input
    |> Array.map (fun s ->
        let [| a; b; c; d |] = s.Split(',', '-')
        (int a, int b), (int c, int d))

let part1 ranges =
    ranges
    |> Array.filter (fun ((a, b), (c, d)) -> (a <= c && b >= d) || (c <= a && d >= b))
    |> Array.length

let part2 ranges =
    ranges
    |> Array.filter (fun ((a, b), (c, d)) ->
        (a <= c && c <= b) || (a <= d && d <= b) || (c <= a && a <= d) || (c <= b && b <= d))
    // |> Array.filter (fun ((a, b), (c, d)) -> a=b)
    |> Array.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
