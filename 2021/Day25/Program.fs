open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun r -> r |> Seq.toArray)
    |> array2D

let moveStep rows cols map =
    // move right
    let map =
        Array2D.mapi (fun r c e ->
            match e with
            | '>' when Array2D.get map r ((c + 1) % cols) = '.' -> '.'
            | '>' -> '>'
            | 'v' -> 'v'
            | '.' when Array2D.get map r ((c - 1 + cols) % cols) = '>' -> '>'
            | '.' -> '.'
            ) map
    // move down
    map
    |> Array2D.mapi (fun r c e ->
        match e with
        | '>' -> '>'
        | 'v' when Array2D.get map ((r + 1) % rows) c = '.' -> '.'
        | 'v' -> 'v'
        | '.' when Array2D.get map ((r - 1 + rows) % rows) c = 'v' -> 'v'
        | '.' -> '.'
        )

let rec repeat count f x =
    let x' = f x
    if x = x' then count + 1 else repeat (count + 1) f x'


let part1 map =
    let cols = Array2D.length2 map
    let rows = Array2D.length1 map

    repeat 0 (moveStep rows cols) map


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
