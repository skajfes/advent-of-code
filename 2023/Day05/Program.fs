open System.IO
open FParsec

let pSeeds = pstring "seeds: " >>. sepBy pint64 (pchar ' ') .>> newline
let pMapLine = pint64 .>> pchar ' ' .>>. pint64 .>> pchar ' ' .>>. pint64 .>> newline |>> fun ((a, b), c) -> (a, b, c)
let pMap = many1Chars asciiLetter .>> pstring "-to-" .>>. many1Chars asciiLetter .>> pstring " map:" .>> newline
           .>>. many1 pMapLine
           .>> skipRestOfLine true
let mapParser = pSeeds .>> newline .>>. many1 pMap

let parse (input: string) =
    input
    |> run mapParser
    |> function
       | Success (x, _, _) -> x
       | _ -> failwith "not parsing"

let inRange seed (dest, src, len) =
    seed >= src && seed < src + len

let transform rules seed =
    match List.tryFind (inRange seed) rules with
    | Some (dest, src, len) -> dest + (seed - src)
    | None -> seed

let findRules maps source =
    maps
    |> List.tryFind (fun ((src, _), _) -> src = source)
    |> function
        | Some ((src, dest), rules) -> Some (dest, rules)
        | None -> None

let rec doTransform maps source seed =
    match findRules maps source with
    | Some (dest, rules) ->
        let seed' = transform rules seed
        doTransform maps dest seed'
    | None -> seed

let part1 (seeds, maps) =
    seeds
    |> List.map (doTransform maps "seed")
    |> List.min

let rec transform' rules (start, stop) =

    // rule starts before seed range
    match List.tryFind (fun (dest, src, len) -> src <= start && src + len > start) rules with
    // seed range whole in here
    | Some (dest, src, len) when stop < src + len -> [(start - src + dest, stop - src + dest)]
    // seed range broken
    | Some (dest, src, len) -> (start - src + dest,  len + dest) :: transform' rules (src + len, stop)
    // no rule - fallback to id
    | None ->
        // find rules in seed range
        match List.tryFind (fun (dest, src, len) -> start <= src && stop >= src) rules with
        | None -> [(start, stop)]
        | Some (dest, src, len) when src + len > stop -> [(start, src); (dest, stop - src + dest)]
        | Some (dest, src, len) -> (start, src)::(dest, dest + len):: transform' rules (src + len, stop)


let rec doTransform' maps source seed =
    // printfn "source %s %A" source seed
    match findRules maps source with
    | Some (dest, rules) ->
        let seed' = transform' rules seed
        seed'
        |> List.map (doTransform' maps dest)
        |> List.concat
    | None -> [seed]

let part2 (seeds, maps) =
    seeds
    |> List.chunkBySize 2
    |> List.map (fun [seed; range] -> seed, seed + range - 1L)
    |> Seq.map (doTransform' maps "seed")
    |> Seq.concat
    |> Seq.map fst
    |> Seq.min

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
