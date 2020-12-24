

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

let countBlackTiles instructions =
    flipAll instructions
    |> Array.length

let neighbourTiles (r, c) =
    // [|[NE];[NW];[SE];[SW];[E];[W]|]|> Array.map (flip t)
    [|(r-1, c); (r - 1, c - 1);  (r, c - 1); (r, c + 1); (r + 1, c); (r + 1, c + 1)|]

let gameOfLife tiles =

    let round tiles =
        let ns =
            tiles
            |> Array.map (fun t -> t, neighbourTiles t)

        let blacks =
            ns
            |> Array.filter (fun (t, ns) ->
                let black_ns =
                    ns
                    |> Array.map (fun t -> Array.contains t tiles)
                    |> Array.filter ((=)true)
                    |> Array.length
                match black_ns with
                | 1 | 2 -> true
                | _ -> false)
            |> Array.map fst

        let newBlacks =
            ns
            |> Array.collect snd
            |> Array.filter (fun t -> Array.contains t tiles |> not)
            |> Array.countBy id
            |> Array.filter (fun (t, count) ->
                match count with
                | 2 -> true
                | _ -> false)
            |> Array.map fst

        Array.append blacks newBlacks

    (tiles, [1..100])
    ||> List.fold (fun ts r -> round ts)
    |> Array.length

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    let testInput = System.IO.File.ReadAllLines("sample.txt")

    let sw = Stopwatch.StartNew()
    input |> parse |> countBlackTiles |> printfn "Part 1: %d"
    let p1 = sw.ElapsedMilliseconds
    sw.Restart()
    input |> parse |> flipAll |> gameOfLife |> printfn "Part 2: %d"
    let p2 = sw.ElapsedMilliseconds
    printfn "Part1: %d ms, Part2: %d ms" p1 p2
    
    0 // return an integer exit code