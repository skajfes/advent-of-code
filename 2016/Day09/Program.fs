open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None


type Token =
    | Repeat of int64 * int64
    | Char
    | Empty

let rec decompress recurse input =
    match input with
    | Regex "^\((\d+)x(\d+)\)(.*)" [chars; repeat; rest] ->
        let sectionLength =
            if recurse
            then Seq.take (int chars) rest |> String.Concat |> decompress recurse
            else int64 chars
        (sectionLength * int64 repeat) + decompress recurse (Seq.skip (int chars) rest |> String.Concat)
    | Regex "^.(.*)" [ rest ] -> 1L + decompress recurse rest
    | "" -> 0L
    | _ -> failwith input


[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt").Trim()

    input |> decompress false |> printfn "Part 1: %d"
    input |> decompress true |> printfn "Part 2: %d"

    0 // return an integer exit code
