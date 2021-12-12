open System.IO
open System.Threading

type Cave =
    // | Start
    // | End
    | Big of string
    | Small of string

let cave (s: string) =
    match s with
    // | "start" -> Small "start"
    // | "end" -> Small "end"
    | s when System.Char.IsUpper(s[0]) -> Big s
    | s -> Small s

let parse (input: string[]) =
    input
    |> Array.map (fun line ->
        let [|a; b|] = line.Split("-")
        (cave a, cave b)
        )
    |> Array.toList

let paths start caves =
    caves
    |> List.filter (fun (a, b) -> a = start || b = start)
    |> List.map (function
                  | a, b when a = start -> b
                  | a, b -> a)

let rec traverse (start :: path) caves =
    match start with
    | Small "end" -> [start :: path]
    | _ ->
    paths start caves
    |> List.filter (fun cave ->
        match cave with
        | Small c -> List.contains cave path |> not
        | _ -> true)
    |> List.map (fun p -> p::start::path)
    |> List.collect (fun path -> traverse path caves)

let isSmall cave =
    match cave with
    | Small _ -> true
    | _ -> false

let rec traverse2 (start :: path) caves =
    match start with
    | Small "end" -> [start :: path]
    | _ ->
    paths start caves
    |> List.filter (fun cave ->
        match cave with
        | Small "start" -> false
        | Small _ ->
            match List.contains cave path with
            | false -> true
            | true ->
                let anyDouble = start::path |> List.filter isSmall |> List.countBy id |> List.exists (fun (c, cnt) -> cnt >= 2)
                not anyDouble
        | _ -> true)
    |> List.map (fun p -> p::start::path)
    |> List.collect (fun path -> traverse2 path caves)

let print (paths: Cave list list) =
    paths
    |> List.map (fun path -> path |> List.rev |> List.map (function | Small c -> c | Big c -> c) |> String.concat " -> ")
    |> String.concat "\n"
    |> printfn "%s"

    printfn "%d" (List.length paths)

let part1 caves =
    caves
    |> traverse [Small "start"]
    // |> print
    |> List.length

let part2 caves =
    caves
    |> traverse2 [Small "start"]
    // |> print
    |> List.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    let testInput = File.ReadAllLines("sample2.txt")

    // testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    // input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
