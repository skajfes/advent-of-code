open System
open System.IO
open System.Text.RegularExpressions

type Item =
    | Chip of string
    | Generator of string

type Position =
    | Floor of int * Item list

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None


let toItem item =
    match item with
    | Regex "a (.*) generator" [element] -> Generator element |> Some
    | Regex "a (.*)-compatible microchip" [element] -> Chip element |> Some
    | Regex "nothing relevant" [] -> None
    | _ -> failwithf "wat %s" item

let unpackItems (itemsLine: string) =
    itemsLine.Split([| ", "; " and "; "." |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun e -> e.Trim())
    |> Array.map toItem
    |> Array.filter (function | Some _ -> true | _ -> false)
    |> Array.map (function | Some x -> x)
    |> Array.toList

let parseLine (line: string) =
    match line with
    | Regex "The (.*) floor contains (.*)" [floor; items] ->
        let unpacked = unpackItems items
        match floor with
        | "first" -> Floor (1, unpacked)
        | "second" -> Floor (2, unpacked)
        | "third" -> Floor (3, unpacked)
        | "fourth" -> Floor (4, unpacked)
    | _ -> failwithf "unknown: %s" line

let parse (input: string[]) =
    input
    |> Array.map (parseLine)

[<EntryPoint>]
let main argv =
    // let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> printfn "%A"
    
    // input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
