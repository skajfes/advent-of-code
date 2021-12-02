open System.IO

type Direction =
    | Forward of int
    | Up of int
    | Down of int

let (|Prefix|_|) (prefix: string) (input: string) =
    if input.StartsWith(prefix)
    then input.Replace(prefix, "").Trim() |> int |> Some
    else None

let parse (input: string[]) =
    input
    |> Array.map (function
                  | Prefix "forward" x -> Forward x
                  | Prefix "up" x -> Up x
                  | Prefix "down" x -> Down x)

let part1 directions =
    ((0, 0), directions)
    ||> Array.fold (fun (horizontal, vertical) d ->
        match d with
        | Forward x -> horizontal + x, vertical
        | Up x -> horizontal, vertical - x
        | Down x -> horizontal, vertical + x)
    |> fun (a, b) -> a * b

let part2 directions =
    ((0,0,0), directions)
    ||> Array.fold (fun (horizontal, vertical, aim) ->
        function
        | Forward x -> horizontal + x, vertical + aim*x, aim
        | Up x -> horizontal, vertical, aim - x
        | Down x -> horizontal, vertical, aim + x)
    |> fun (a, b, _) -> a * b


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
