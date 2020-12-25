open System.Diagnostics

let (|Prefix|_|) (prefix: string) (input: string) =
    if input.StartsWith(prefix) then Some <| input.[prefix.Length..]
    else None

type Direction = SE | SW | NE | NW | E | W
let rec parseLine (line: string) =
    match line with
    | "" -> []
    | Prefix "se" rest -> SE :: parseLine rest
    | Prefix "sw" rest -> SW :: parseLine rest
    | Prefix "nw" rest -> NW :: parseLine rest
    | Prefix "ne" rest -> NE :: parseLine rest
    | Prefix "e" rest -> E :: parseLine rest
    | Prefix "w" rest -> W :: parseLine rest
    | _ -> failwith "unknown input"

let parse = Array.map parseLine

let flip start instruction =
    (start, instruction)
    ||> List.fold (fun (r, c) ->
        function
        | SE -> (r + 1, c + 1)
        | SW -> (r + 1, c - 0)
        | E -> (r, c + 1)
        | W -> (r, c - 1)
        | NE -> (r - 1, c + 0)
        | NW -> (r - 1, c - 1))

let flipAll instructions =
    instructions
    |> Array.map (flip (0, 0))
    |> Array.countBy id
    |> Array.filter (fun t -> snd t % 2 = 1)
    |> Array.map fst
    |> Set.ofArray

let countBlackTiles instructions =
    flipAll instructions
    |> Set.count

let foldi (f: 'b -> 'a -> 'b) (init: 'b) (data: 'a[,]) =
    let mutable res = init
    for i in [0..Array2D.length1 data - 1] do
        for j in [0..Array2D.length2 data - 1] do
            res <- f res (data.[i,j])
    res

let printMap tiles =
    let mutable res = ""
    for i in [0..Array2D.length1 tiles - 1] do
        for j in [0..Array2D.length2 tiles - 1] do
            res <- res + if tiles.[i,j] then "#" else " "
        res <- res + "\n"
    System.Console.SetCursorPosition(0, 0)
    printfn "%s" res

let gameOfLife tiles =

    let round (tiles: bool[,]) =
        // printMap tiles
        tiles
        |> Array2D.mapi (fun i j t ->
            // don't check edges of array
            if i = 0 || j = 0 || i = (Array2D.length1 tiles - 1) || j = (Array2D.length2 tiles - 1) then false else
            // all neighbours
            [|(i-1, j); (i - 1, j - 1);  (i, j - 1); (i, j + 1); (i + 1, j); (i + 1, j + 1)|]
            |> Array.filter (fun (i, j) -> tiles.[i, j])
            |> Array.length
            |> fun l ->
                match t, l with
                | true, l when l = 1 || l = 2 -> true
                | false, 2 -> true
                | _ -> false)

    (tiles, [1..100])
    ||> List.fold (fun ts r -> round ts)
    |> foldi (fun sum c -> sum + if c then 1 else 0) 0

let createMap blacks =
    Array2D.init 150 150 (fun i j -> Set.contains (i-75, j-75) blacks)

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    let testInput = System.IO.File.ReadAllLines("sample.txt")

    let sw = Stopwatch.StartNew()
    input |> parse |> countBlackTiles |> printfn "Part 1: %d"
    let p1 = sw.ElapsedMilliseconds
    sw.Restart()
    input |> parse |> flipAll |> createMap |> gameOfLife |> printfn "Part 2: %d"
    let p2 = sw.ElapsedMilliseconds
    printfn "Part1: %d ms, Part2: %d ms" p1 p2
    
    0 // return an integer exit code