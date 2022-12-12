open System.IO
open System.Text.RegularExpressions

type Command =
    | D of int
    | U of int
    | L of int
    | R of int

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some <| List.tail [ for g in m.Groups -> g.Value ]
    else
        None

let parse (input: string[]) =
    input
    |> Array.map (function
        | Regex "D (\d+)" [ x ] -> D(int x)
        | Regex "U (\d+)" [ x ] -> U(int x)
        | Regex "L (\d+)" [ x ] -> L(int x)
        | Regex "R (\d+)" [ x ] -> R(int x))

let move_head cmd ((x, y)::heads) =
    match cmd with
    | R a -> [for dx in 0..a do (x + dx, y)]
    | L a -> [for dx in 0..a do (x - dx, y)]
    | U a -> [for dy in 0..a do (x, y + dy)]
    | D a -> [for dy in 0..a do (x, y - dy)]
    |> List.rev
    |> fun x -> List.append x heads

let move_tail tail head =
    match tail, head with
    | (x, y), (x2, y2) when y = y2 && x2 > x -> (x + 1, y)
    | (x, y), (x2, y2) when y = y2 && x2 < x -> (x - 1, y)
    | (x, y), (x2, y2) when x = x2 && y2 > y -> (x, y + 1)
    | (x, y), (x2, y2) when x = x2 && y2 < y -> (x, y - 1)
    | (x, y), (x2, y2) when y2 > y && x2 > x -> (x + 1, y + 1)
    | (x, y), (x2, y2) when y2 > y && x2 < x -> (x - 1, y + 1)
    | (x, y), (x2, y2) when y2 < y && x2 > x -> (x + 1, y - 1)
    | (x, y), (x2, y2) when y2 < y && x2 < x -> (x - 1, y - 1)
    | _ -> failwith "no"

let in_range (xa, ya) (xb, yb) =
    abs (xb - xa) <= 1 && abs (yb - ya) <= 1

let rec visit head (tail :: tails) =
    if in_range tail head then
        tail :: tails
    else
        visit head (move_tail tail head :: tail :: tails)

let move n commands =
    let tails = (Map.empty, [ 0..n ]) ||> List.fold (fun m c -> Map.add c [ (0, 0) ] m)

    //move head
    let tails =
        (tails, commands)
        ||> Array.fold (fun tails cmd ->
            Map.add 0 (move_head cmd tails.[0]) tails
        )

    (tails, [1..n])
    ||> List.fold (fun tails n ->
        (tails.[n], tails.[n-1] |> List.rev)
        ||> List.fold (fun tail_n head ->
            visit head tail_n
        )
        |> fun tail_n -> Map.add n tail_n tails
    )
    |> Map.find n
    |> List.distinct
    |> List.length

let part1 commands = move 1 commands
let part2 commands = move 9 commands

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample2.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
