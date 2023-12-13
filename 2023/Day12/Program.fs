open System.Collections.Generic
open System.IO

type Groups =
    | Start of int
    | Continue of int

let parse (input: string[]) = 
    input
    |> Array.map (_.Split(' '))
    |> Array.map (fun [| a; b |] -> a |> Seq.toList, b.Split(',') |> Array.map (int>>Start) |> Array.toList)
    |> Array.toList

let dict = Dictionary<(char list*Groups list), int64>()
let rec arrangements (input, groups) =
    if dict.ContainsKey(input, groups) then dict[input, groups] else
    let value =
        match input, groups with
        | [], [] -> 1L
        | [], [Continue 0] -> 1L
        | [], _ -> 0L
        | '.'::ins, [] -> arrangements (ins, [])
        | '?'::ins, [] -> arrangements (ins, []) // always like .
        | '.'::ins, Start _::_ -> arrangements (ins, groups)
        | '#'::ins, Start x::gs -> arrangements (ins, Continue (x-1) :: gs)
        | '#'::ins, [] -> 0L
        | '#'::ins, Continue 0::gs -> 0L
        | '#'::ins, Continue x::gs when x > 0 -> arrangements (ins, Continue (x-1) :: gs)
        | '.'::ins, Continue 0::gs -> arrangements (ins, gs)
        | '.'::ins, Continue x::gs -> 0L
        | '?'::ins, Continue 0::gs -> arrangements (ins, gs) // always like .
        | '?'::ins, Continue x::gs -> arrangements (ins, Continue (x - 1):: gs) // always like #
        | '?'::ins, Start x :: gs ->
            let one = arrangements (ins, groups) // like .
            let two = arrangements (ins, Continue (x-1)::gs) // like #
            one + two
        // | _ -> 0
        | x -> failwithf "match failed for %A" x

    dict.Add((input, groups), value)
    value

let part1 input =
    input
    |> List.map arrangements
    |> List.sum

let rec replicate n list =
    match n with
    | 1 -> list
    | x -> list |> List.append ['?'] |> List.append (replicate (x-1) list)

let unfold input =
    input
    |> List.map (fun (ins, gs) -> replicate 5 ins, gs |> List.replicate 5 |> List.concat)

let part2 input =
    input
    |> unfold
    |> List.map arrangements
    |> List.sum


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
