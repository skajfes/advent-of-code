open System.IO

let parse (input: string[]) =
    input

type SyntaxCheck =
    | Ok
    | Error of char

let flip brace =
    match brace with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwithf "unknown brace %c" brace

let parseLine (line: string) =
    ((Ok, []), line)
    ||> Seq.fold (fun (result, openBraces) char ->
            match result with
            | Error _ -> result, []
            | Ok ->
                match char, openBraces with
                | x, _ when "({[<".Contains(x) -> Ok, (flip x)::openBraces
                | closed, opened::rest when closed = opened -> Ok, rest
                | _ -> Error char, []
        )

let part1 (input: string[]) =
    input
    |> Array.map parseLine
    |> Array.map fst
    |> Array.sumBy (function
            | Error ')' -> 3
            | Error ']' -> 57
            | Error '}' -> 1197
            | Error '>' -> 25137
            | _ -> 0 )

let getScore input =
    (0L, input)
    ||> Seq.fold (fun score brace ->
            match brace with
            | ')' -> 1L
            | ']' -> 2L
            | '}' -> 3L
            | '>' -> 4L
            |> (+) (score*5L)
        )
let part2 (input: string[]) =
    let scores =
        input
        |> Array.map parseLine
        |> Array.filter (function | Ok, _ -> true | _ -> false)
        |> Array.map snd
        |> Array.map (List.map string >> String.concat "")
        |> Array.map getScore
        |> Array.sort
    let len = Array.length scores
    scores.[len/2]


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
