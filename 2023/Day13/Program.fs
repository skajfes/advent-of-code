open System
open System.IO

let parse (input: string) =
    input.ReplaceLineEndings("|").Split("||", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (_.Split("|", StringSplitOptions.RemoveEmptyEntries) >> Array.map Seq.toArray)

let rec areEqual (xs, ys) =
    match xs, ys with
    | [||], _ -> true
    | _, [||] -> true
    | xs, ys when Array.head xs = Array.head ys -> areEqual (Array.tail xs, Array.tail ys)
    | _ -> false

let checkReflection map i =
    map
    |> Array.splitAt (i + 1)
    |> fun (a, b) -> Array.rev a, b
    |> areEqual


let toString (map: char array array) =
    map
    |> Array.map (fun r -> String(r))
    |> String.concat "\n"

let findMirror skipLine map =
    // printfn "%s" (toString map)

    map
    |> Array.pairwise
    |> Array.indexed
    |> Array.filter (fun (i, (a, b)) -> a = b) // possible reflection points
    // |> fun x ->
    //     printfn "%A" x
    //     x
    |> Array.map fst
    |> Array.filter ((<>)skipLine)
    |> Array.filter (checkReflection map)
    |> Array.map ((+)1)
    |> Array.tryHead

let findMirrorLine lastScore map =
    let skipRow, skipColumn =
        match lastScore with
        | x when x >= 100 -> lastScore / 100 - 1, -1
        | _ -> -1, lastScore - 1

    match findMirror skipRow map with
    | Some c -> 100 * c
    | None ->
        match findMirror skipColumn (Array.transpose map) with
        | Some c -> c
        | None -> 0

let part1 maps =
    maps
    |> Array.map (findMirrorLine 0)
    |> Array.sum

let copy (map: char array array) =
    map
    |> Array.map (Array.copy)

let createSmudges (map: char array array) =
    map
    |> Array.indexed
    |> Array.fold (fun res (r, row) ->
        row
        |> Array.indexed
        |> Array.fold (fun res (c, v) ->
            let copy = Array.map Array.copy map
            copy[r][c] <- match v with | '#' -> '.' | _ -> '#'
            // printfn "%A" copy
            copy::res
            ) res) []
    |> List.toArray
    |> Array.rev

let part2 maps =
    maps
    |> Array.map (fun m -> m, findMirrorLine 0 m, createSmudges m)
    |> Array.map (fun (m, l, maps) ->
           maps
           |> Array.map (findMirrorLine l)
           |> Array.filter (fun ll -> ll > 0 && l <> ll)
           |> Array.tryHead
           |> (function | Some x -> x | _ -> l))
    |> Array.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample2.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
