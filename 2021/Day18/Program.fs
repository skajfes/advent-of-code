open System.IO
open FParsec
open Microsoft.FSharp.Collections

type Pair =
    | Value of int
    | Pair of Pair * Pair

let run p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let parsePair, ppref = createParserForwardedToRef<Pair, unit>()
let pvalue = (pint32 |>> Value) <|> parsePair
ppref.Value <- pstring "[" >>. pvalue .>>. (pstring "," >>. pvalue) .>> pstring "]" |>> Pair

let parse (input: string[]) =
    input
    |> Array.map (run parsePair)
    |> Array.toList

let rec toString pair =
    match pair with
    | Value x -> string x
    | Pair (a, b) -> "[" + toString a + "," + toString b + "]"

let print pair =
    toString pair |> printfn "%A"

let has4nested pair =
    let rec isNested depth pair =
        if depth > 4 then true else
            match pair with
            | Value _ -> false
            | Pair(a, b) ->
                isNested (depth + 1) a || isNested (depth + 1) b
    isNested 0 pair

let rec has10 pair =
    match pair with
    | Value x when x >= 10 -> true
    | Value _ -> false
    | Pair(a, b) ->
        has10 a || has10 b


let rec addLeft pair value =
    match pair with
    | Pair (Value a, p) -> Pair (Value (a + value), p)
    | Pair (p1, p2) -> Pair(addLeft p1 value, p2)
    | Value a -> Value (a + value)

let rec addRight pair value =
    match pair with
    | Pair (p, Value a) -> Pair (p, Value (a + value))
    | Pair (p1, p2) -> Pair (p1, addRight p2 value)
    | Value a -> Value (a + value)

let explode pair =
    // printfn "exploding %A" (toString pair)
    let rec isNested depth pair =
        if depth >= 4
        then
            match pair with
            | Pair (Value a, Value b) -> true, Value 0, a, b
            | Value x -> false, Value x, 0, 0
            | _ -> failwith "unsupported explode"
        else
            match pair with
            | Value _ -> false, pair, 0, 0
            | Pair(a, b) ->
                match isNested (depth + 1) a with
                | true, pair, addA, addB -> true, Pair(pair, addLeft b addB), addA, 0
                | false, pair, addA, addB ->
                    match isNested (depth + 1) b with
                    | true, pair, addA, addB -> true, Pair (addRight a addA, pair), 0, addB
                    | false, pair, addA, addB -> false, pair, addA, addB

    match isNested 0 pair with
    | success, pair, _, _ -> pair

let ff f x = float x / 2.0 |> f |> int
let split pair =
    // printfn "splitting %A" (toString pair)

    let rec dosplit pair =
        match pair with
        | Value x when x >= 10 -> true, Pair (ff floor x |> Value, ff ceil x |> Value)
        | Value x -> false, Value x
        | Pair(a, b) ->
            match dosplit a with
            | true, pair -> true, Pair(pair, b)
            | false, pair ->
                match dosplit b with
                | x, pair -> x, Pair(a, pair)

    dosplit pair |> snd

let rec reduce pair =
    if has4nested pair then explode pair |> reduce
    else if has10 pair then split pair |> reduce
    else pair

let rec magnitude pair =
    match pair with
    | Value x -> x
    | Pair (a, b) -> 3*(magnitude a) + 2*(magnitude b)

let part1 (p::pairs) =
    (p, pairs)
    ||> List.fold (fun sum p -> reduce (Pair(sum, p)))
    |> magnitude

let rec allPairs list =
    match list with
    | [] -> []
    | a::list ->
        list
        |> List.map (fun b -> [Pair(a, b); Pair(b, a)])
        |> List.concat
        |> List.append (allPairs list)

let part2 pairs =
    pairs
    |> allPairs
    |> List.map (reduce>>magnitude)
    |> List.max

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let sample = File.ReadAllLines("sample.txt")

    let pairs = parse input
    pairs |> part1 |> printfn "Part 1: %A"
    pairs |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
