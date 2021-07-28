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

let token input =
    match Seq.tryHead input with
    | Some '(' ->
        let chars = Seq.takeWhile ((<>) 'x') (Seq.tail input) |> String.Concat
        let count = input |> Seq.skip (2 + chars.Length) |> Seq.takeWhile ((<>)')') |> String.Concat

        Repeat (int64 chars, int64 count), Seq.skip (Seq.length chars + Seq.length count + 3) input
    | Some _ -> Char, Seq.tail input
    | None -> Empty, Seq.empty


let rec decompress2 recurse input =
    match token input with
    | Empty, _ -> 0L
    | Char, rest -> 1L + decompress2 recurse rest
    | Repeat (chars, count), rest ->
        let sectionLength =
            if recurse
            then Seq.take (int chars) rest |> decompress2 recurse
            else chars
        (count * sectionLength) + decompress2 recurse (Seq.skip (int chars) rest)


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
    input |> decompress2 false |> printfn "Part 1: %d"
    input |> decompress2 true |> printfn "Part 2: %d"

    0 // return an integer exit code
