open System.IO

type Roll =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Lose
    | Draw
    | Win

let toRoll a =
    match a with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> failwithf "no: %s" a

let parse (input: string[]) =
    input
    |> Array.map (fun line ->
        match line.Split(" ") with
        | [| a; b |] -> (toRoll a, toRoll b)
        | _ -> failwith "unknown"
    )

let score (a, b) =
    match b, a with
    | Rock, Scissors -> 1 + 6
    | Rock, Paper -> 1 + 0
    | Rock, Rock -> 1 + 3
    | Paper, Paper -> 2 + 3
    | Paper, Rock -> 2 + 6
    | Paper, Scissors -> 2 + 0
    | Scissors, Rock -> 3 + 0
    | Scissors, Paper -> 3 + 6
    | Scissors, Scissors -> 3 + 3

let score2 (a, b) =
    match a, b with
    | Rock, Lose -> 3 + 0
    | Rock, Draw -> 1 + 3
    | Rock, Win -> 2 + 6
    | Paper, Lose -> 1 + 0
    | Paper, Draw -> 2 + 3
    | Paper, Win -> 3 + 6
    | Scissors, Lose -> 2 + 0
    | Scissors, Draw -> 3 + 3
    | Scissors, Win -> 1 + 6


let part1 rounds =
    rounds
    // |> Array.map (fun x -> x, score x)
    |> Array.map score
    |> Array.sum

let toOutcome x =
    match x with
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "dlfka"

let part2 (input: string[]) =
    input
    |> Array.map (fun line ->
        match line.Split(" ") with
        | [| a; b |] -> (toRoll a, toOutcome b)
        | _ -> failwith "unknown"
    )
    // |> Array.map (fun x -> x, score2 x)
    |> Array.map score2
    |> Array.sum



[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
