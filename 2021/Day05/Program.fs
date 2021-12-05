open System.IO
open System.Text.RegularExpressions

let parseLine (line: string) =
    let m = Regex.Match(line, @"(\d+),(\d+) -> (\d+),(\d+)")

    match m.Success, m.Groups with
    | true, matches ->
        matches
        |> Seq.tail
        |> Seq.map (fun x -> int x.Value)
        |> Seq.toList
        |> fun [ a; b; c; d ] -> (a, b), (c, d)
    | false, _ -> failwith "not a valid line"

let parse (input: string []) =
    input |> Array.map parseLine

let expandLine ((x1, y1), (x2, y2)) =
    match (x1, y1), (x2, y2) with
    | _ when x1 = x2 && y2 > y1 -> [| for y in y1 .. y2 -> x1, y |]
    | _ when x1 = x2            -> [| for y in y2 .. y1 -> x1, y |]
    | _ when x1 <= x2           -> [| for x in 0..(x2 - x1) -> x + x1, y1 + x * (System.Math.Sign(int y2 - y1)) |]
    | _                         -> [| for x in 0..(x1 - x2) -> x + x2, y2 + x * (System.Math.Sign(int y1 - y2)) |]

let countIntersections lines =
    lines
    // |> Array.map (fun l -> l, expandLine l)
    |> Array.collect expandLine
    |> Array.countBy id
    |> Array.filter (snd >> (<=)2)
    |> Array.length

let part1 lines =
    lines
    |> Array.filter
        (function
        | (x1, y1), (x2, y2) when x1 = x2 -> true
        | (x1, y1), (x2, y2) when y1 = y2 -> true
        | _ -> false)
    |> countIntersections

let part2 lines = lines |> countIntersections

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
