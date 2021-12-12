open System.IO

type Cave =
    | Big of string
    | Small of string

let cave (s: string) =
    match s with
    | s when System.Char.IsUpper(s[0]) -> Big s
    | s -> Small s

let parse (input: string[]) =
    input
    |> Array.map (fun line ->
        let [|a; b|] = line.Split("-")
        [(cave a, cave b); (cave b, cave a)]
        )
    |> Array.toList
    |> List.concat
    |> List.groupBy fst
    |> List.map (fun (a, caves) -> a, caves |> List.map snd)
    |> Map.ofList

let paths start (caves: Map<Cave, Cave list>) =
    caves[start]

let print (path: Cave list) =
    path
    |> List.rev
    |> List.map (function
                | Small c -> c
                | Big c -> c)
    |> String.concat " -> "
    |> printfn "%s"

let rec traverse (start :: path) used caves =
    match start with
    | Small "end" ->
        // print (start::path)
        1
    | _ ->
        paths start caves
        |> List.map (fun cave ->
            match cave, used with
            | Small "start", _ -> cave, false, used
            | Small _, Some u ->
                match List.contains cave path with
                | false -> cave, true, used
                | true -> cave, not u, Some true
            | Small _, None ->
                match List.contains cave path with
                | false -> cave, true, None
                | true -> cave, false, None
            | _ -> cave, true, used)
        |> List.filter (fun (_, a, _) -> a)
        |> List.sumBy (fun (cave, _, used) -> traverse (cave::start::path) used caves)


let part1 caves =
    caves
    |> traverse [Small "start"] None

let part2 caves =
    caves
    |> traverse [Small "start"] (Some false)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    let testInput = File.ReadAllLines("sample2.txt")

    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
