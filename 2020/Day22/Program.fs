open System.Collections.Generic
open System.Diagnostics
open System.IO
let testInput = File.ReadAllLines("sample.txt")

let parse (input: string[]) =
    let parseDeck list =
        list
        |> Array.tail
        |> Array.map int
        |> Array.toList

    let p1 =
        input
        |> Array.takeWhile ((<>)"")
    let p2 =
        input
        |> Array.skip (Array.length p1 + 1)

    parseDeck p1, parseDeck p2

let rec playRound p1 p2 =
    match p1, p2 with
    | _, [] -> p1
    | [], _ -> p2
    | c1::p1, c2::p2 when c1 > c2 -> playRound (p1@[c1;c2]) p2
    | c1::p1, c2::p2 when c1 < c2 -> playRound p1 (p2@[c2;c1])
    | _ -> failwith "same cards"

type Winner =
    | P1 of int list
    | P2 of int list


let rec playRecursiveGame p1 p2 =
    let visited = HashSet()

    let rec playRecursiveRound p1 p2 =
        if visited.Contains(p1, p2) then P1 p1 else
        visited.Add (p1, p2) |> ignore


        match p1, p2 with
        | _, [] -> P1 p1
        | [], _ -> P2 p2
        | c1 :: p1, c2 :: p2 when c1 <= List.length p1 && c2 <= List.length p2 ->
            // recursive round
            match playRecursiveGame (p1.[0..c1 - 1]) (p2.[0..c2 - 1]) with
            | P1 _ -> playRecursiveRound (p1 @ [ c1; c2 ]) p2
            | P2 _ -> playRecursiveRound p1 (p2 @ [ c2; c1 ])
        | c1 :: p1, c2 :: p2 when c1 > c2 -> playRecursiveRound (p1 @ [ c1; c2 ]) p2
        | c1 :: p1, c2 :: p2 when c1 < c2 -> playRecursiveRound p1 (p2 @ [ c2; c1 ])
        | _ -> failwith "same cards"
    playRecursiveRound p1 p2

let calculateScore deck =
    deck
    |> List.zip [int64 (List.length deck) .. -1L .. 1L]
    |> List.map (fun (c, s) -> c * int64 s)
    |> List.sum

let unwrap =
    function
    | P1 deck -> deck
    | P2 deck -> deck

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    
    // testInput |> parse |> printfn "%A"

    let sw = Stopwatch.StartNew()
    input |> parse ||> playRound |> calculateScore |> printfn "Part 1: %d"
    let p1 = sw.ElapsedMilliseconds
    sw.Restart()
    input |> parse ||> playRecursiveGame |> unwrap |> calculateScore |> printfn "Part 2: %d"
    let p2 = sw.ElapsedMilliseconds
    sw.Stop()
    printfn $"p1: {p1}\tp2: {p2}"

    0 // return an integer exit code