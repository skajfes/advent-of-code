open System.IO
open FParsec

type Direction = R | L

let pInstruction = pchar 'R' <|> pchar 'L'
                    |>> function
                        | 'R' -> R
                        | 'L' -> L
let pInstructions = many1 pInstruction .>> newline
let pLine =
    anyString 3 .>> pstring " = "
    .>> pchar '(' .>>. anyString 3
    .>> pstring ", " .>>. anyString 3 .>> pchar ')' .>> newline
    |>> fun ((a, b), c) -> a, b, c

let pFile = pInstructions .>> newline .>>. many1 pLine .>> eof

let parse (input: string) =
    input
    |> run pFile
    |> function
        | Success (x, _, _) -> x
        | Failure _ -> failwith "not parsing"

let follow directions map start stop =

    let rec do_follow state (d::dirs) steps =
        if stop state then steps else

        let _, left, right = map |> List.find (fun (s, _, _) -> s = state)

        // restart directions if empty
        let dirs = match dirs with
                    | [] -> directions
                    | _ -> dirs

        match d with
        | L -> do_follow left dirs (steps + 1)
        | R -> do_follow right dirs (steps + 1)

    do_follow start directions 0

let part1 (directions, map) =
    follow directions map "AAA" ((=)"ZZZ")


let rec gcd a b =
    if a = 0L then b else
    gcd (b % a) a

let rec lcm (arr: int64 list) =
    match arr with
    | [x] -> x
    | a::arr ->
        let b = lcm arr
        (a*b/gcd a b)

let part2 (directions, map) =

    let starts = map
                 |> List.filter (fun (s:string, _, _) -> s[2] = 'A')
                 |> List.map (fun (s, _, _) -> s)

    let stop (state: string) = state[2] = 'Z'

    starts
    |> List.map (fun s -> follow directions map s stop)
    |> List.map int64
    |> lcm

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample.txt")
    let testInput2 = File.ReadAllText("sample2.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput2 |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
