open System.IO
open FParsec

// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green

type Color =
    | Red
    | Green
    | Blue

type Cube = int * Color
type Game = int * Cube list list

let pColor =
    choice
        [| stringReturn "red" Red
           stringReturn "green" Green
           stringReturn "blue" Blue |]

let pCube = pint32 .>> spaces .>>. pColor |>> Cube
let pDraw = sepBy1 pCube (pstring ", ")
let pDraws = sepBy1 pDraw (pstring "; ")

let pGame =
    pstring "Game " >>. pint32 .>> pstring ": " .>>. pDraws
    |>> fun (g, d) -> Game(g, d)

let parse (input: string[]) =
    input
    |> Array.map (
        run pGame
        >> function
            | Success(x, _, _) -> x
            | _ -> failwith "not parsing"
    )

let overdraw red green blue =
    function
    | v, Red -> v > red
    | v, Green -> v > green
    | v, Blue -> v > blue

let possibleGame red green blue (game: Game) =
    snd game |> List.concat |> List.filter (overdraw red green blue) |> List.isEmpty

let part1 (games: Game array) =
    games |> Array.filter (possibleGame 12 13 14) |> Array.sumBy fst

let minCubes (game: Game) =
    ((0, 0, 0), snd game |> List.concat)
    ||> List.fold (fun (r, g, b) cube ->
        match cube with
        | v, Red when v > r -> v, g, b
        | v, Green when v > g -> r, v, b
        | v, Blue when v > b -> r, g, v
        | _ -> r, g, b)

let power (r, g, b) = r * g * b

let part2 (games: Game array) =
    games |> Array.map (minCubes) |> Array.sumBy power

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
