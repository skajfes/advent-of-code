open System.IO
open FParsec

let parse (input: string) =
    input.Trim().Split(",")

let hash (input: string) =
    (0, input)
    ||> Seq.fold (fun current ch ->
        current
        |> (+) (int ch)
        |> (*) 17
        |> fun x -> x % 256)

let part1 input =
    input
    |> Array.map hash
    |> Array.sum

type Instruction =
    | Remove
    | Replace of int

let pLabel = many1Chars asciiLower
let pRemove = pchar '-' |>> fun _ -> Remove
let pReplace = pchar '=' >>. pint32 |>> fun focal_length -> Replace focal_length
let pAction = pRemove <|> pReplace
let pRow = pLabel .>>. pAction

let to_instruction (input: string) =
    match run pRow input with
    | Success (x, _, _) -> x
    | _ -> failwith "nope"

let focusing_power boxes =
    boxes
    |> Map.map (fun box lenses ->
        lenses
        |> List.rev
        |> List.mapi (fun i (_, focal_length) -> (1 + box) * (1 + i) * focal_length)
        |> List.sum)
    |> Map.values
    |> Seq.sum

let part2 input =
    input
    |> Array.map to_instruction
    |> Array.fold (fun boxes (label, instruction) ->
        let box_id = hash label
        let lenses =
            boxes
            |> Map.tryFind box_id
            |> function | Some lenses -> lenses | None -> []
        match instruction with
        | Remove -> lenses |> List.filter (fst >> (<>)label)
        | Replace focal ->
            match lenses |> List.tryFind (fst >> (=)label) with
            | Some _ -> lenses |> List.map (function
                                          | lbl, _ when lbl = label -> lbl, focal
                                          | x -> x)
            | None -> (label, focal) :: lenses
        |> fun lenses -> Map.add box_id lenses boxes
        ) Map.empty
    |> focusing_power

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
