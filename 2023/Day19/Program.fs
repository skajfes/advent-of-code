open System.IO
open FParsec

type Part = {
    x: int
    m: int
    a: int
    s: int
}
type Result =
    | Accept
    | Reject
    | Workflow of string
type Rule =
    | Result of Result
    | GreaterThan of string * int * Result
    | LessThan of string * int * Result
type Workflow = string * Rule list
let presult = pstring "A" <|> pstring "R" <|> many1Chars asciiLower
              |>> function
                  | "A" -> Accept
                  | "R" -> Reject
                  | x -> Workflow x
let prule = opt (anyOf "xmas" .>>.? (pchar '<' <|> pchar '>') .>>.? pint32 .>>? pchar ':') .>>. presult
            |>> function
                | Some ((c, '<'), x), res -> LessThan (string c, x, res)
                | Some ((c, '>'), x), res -> GreaterThan (string c, x, res)
                | None, res -> Result res
let pworkflow = many1Chars asciiLower .>>. between (pchar '{') (pchar '}') (sepBy1 prule (pchar ',')).>> newline

let ppart = between (pchar '{') (pchar '}') ( sepBy1 (asciiLower .>> pchar '=' .>>. pint32) (pchar ',') ) .>> newline
            |>> fun [(_, x); (_, m); (_, a); (_, s)] -> { x = x; m = m; a = a; s = s }
let prules = many1 pworkflow .>> newline .>>. many1 ppart .>> eof

let parse (input: string) =
    match run prules input with
    | Success ((workflows, parts), b, c) -> workflows |> Map.ofList, parts
    | Failure (a, _, _) -> failwithf "%A" a


let value part p =
    match p with
    | "x" -> part.x
    | "m" -> part.m
    | "a" -> part.a
    | "s" -> part.s

let run_workflow workflows part =

    let rec evaluate_workflow name =
        let wf = Map.find name workflows
        (None, wf)
        ||> List.fold (fun res rule ->
            match res with
            | Some _ -> res
            | None ->
            match rule with
            | LessThan (p, v, r) when value part p < v -> Some r
            | GreaterThan (p, v, r) when value part p > v -> Some r
            | Result r -> Some r
            | _ -> None)
        |> function
            | Some (Workflow x) -> evaluate_workflow x
            | Some x -> x
            | None -> failwith "nope"

    evaluate_workflow "in"
    |> function
       | Accept -> true
       | Reject -> false
       | _ -> failwith "oh no"

let xmas_rating part = part.x + part.m + part.a + part.s
let part1 workflows parts =
    parts
    |> List.filter (run_workflow workflows)
    |> List.sumBy xmas_rating

let combos (x, m, a, s) =
    [x; m; a; s ]
    |> List.map (fun (min, max) -> max - min + 1)
    |> List.fold (fun acc v -> acc * int64 v) 1L

let less_than value (min, max) =
    if min < value && value < max then
        (min, value - 1), (value, max)
    elif value <= min then
        (min, min-1), (min, max)
    else
        (min, max), (max, max-1)

let greater_than value (min, max) =
    if min < value && value < max then
        (value + 1, max), (min, value)
    elif value < min then
        (min, max), (max, max)
    else
        (max, max), (min, max)

let split f part value (x,m,a,s) =
    match part with
    | "x" -> f value x |> fun (x1, x2) -> (x1,m,a,s), Some (x2,m,a,s)
    | "m" -> f value m |> fun (m1, m2) -> (x,m1,a,s), Some (x,m2,a,s)
    | "a" -> f value a |> fun (a1, a2) -> (x,m,a1,s), Some (x,m,a2,s)
    | "s" -> f value s |> fun (s1, s2) -> (x,m,a,s1), Some (x,m,a,s2)
    | _ -> failwith "no"

let intervals workflows =
    let rec evaluate_workflow name ranges =
        let wf = Map.find name workflows
        ((0L, Some ranges), wf)
        ||> List.fold (fun (accepted, ranges) rule ->
            match ranges with
            | None -> accepted, None
            | Some ranges ->
            match rule with
            | LessThan (part, value, result) -> result, split less_than part value ranges
            | GreaterThan (part, value, result) -> result, split greater_than part value ranges
            | Result r -> r, (ranges, None)
            |> function
                | Workflow wf, (rule_range, leftover_range) -> accepted + evaluate_workflow wf rule_range, leftover_range
                | Accept, (rule_range, leftover_range) -> accepted + combos rule_range, leftover_range
                | Reject, (rule_range, leftover_range) -> accepted, leftover_range
            )
        |> fst

    evaluate_workflow "in" ((1,4000), (1, 4000), (1, 4000), (1,4000))

let part2 workflows parts =
    workflows
    |> intervals

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample.txt")
    
    testInput |> parse ||> part1 |> printfn "%A"
    testInput |> parse ||> part2 |> printfn "%d"

    input |> parse ||> part1 |> printfn "Part 1: %A"
    input |> parse ||> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
