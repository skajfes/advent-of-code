open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

type Action =
    | On of int * int
    | Row of int * int
    | Col of int * int

let parseCommand (input: string) =
    match input with
    | Regex "rect (\d+)x(\d+)" [a; b] -> On (int a, int b)
    | Regex "rotate column x=(\d+) by (\d+)" [c; a] -> Col (int c, int a)
    | Regex "rotate row y=(\d+) by (\d+)" [r; a] -> Row (int r, int a)
    | _ -> failwith "UNKNOWN"

let parse (input: string[]) =
    input
    |> Array.map parseCommand

let draw (rows, cols) cmd field =
    Console.SetCursorPosition(0,0)
    printfn $"%A{cmd}"
    [for x in 0..rows-1 ->
        [for y in 0..cols-1 ->
            match List.contains(x, y) field with
            | true -> "█"
            | false -> " "
        ] |> String.concat ""
    ] |> String.concat "\n" |> printfn "%s"
    // Console.ReadKey() |> ignore
    Threading.Thread.Sleep(30)
    field

let transform (rows, cols) field action =
    match action with
    | On (c, r) ->
        field
        |> List.filter (fun (ri, ci) -> ri >= r || ci >= c)
        |> List.append [for ri in 0..r-1 do for ci in 0..c-1 -> (ri, ci)]
    | Row (r, d) ->
        field
        |> List.map (
            function
            | ri, ci when ri = r -> ri, (ci + d) % cols
            | ri, ci -> ri, ci )
    | Col (c, d) ->
        field
        |> List.map (
            function
            | ri, ci when ci = c -> (ri + d) % rows, ci
            | ri, ci -> ri, ci)
    |> draw (rows, cols) action



let part1 (rows, cols) commands =
    ([], commands)
    ||> Array.fold (transform (rows, cols))
    |> List.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 (3, 7) |> printfn "%A"
    
    input |> parse |> part1 (6, 50) |> printfn "Part 1: %A"
    // Part 2 - look at the output

    0 // return an integer exit code
