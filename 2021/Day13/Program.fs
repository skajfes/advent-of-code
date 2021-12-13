open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

type Fold =
    | FoldX of int
    | FoldY of int

let parse (input: string[]) = 
    (([], []), input)
    ||> Array.fold (fun (paper, folds) row ->
        match row with
        | Regex "fold along x=(\d+)" [x] -> paper, FoldX (int x) :: folds
        | Regex "fold along y=(\d+)" [y] -> paper, FoldY (int y) :: folds
        | Regex "(\d+),(\d+)" [x; y] -> (int x, int y) :: paper, folds
        | _ -> paper, folds)
    |> (fun (paper, folds) -> paper, List.rev folds)

let print paper =
    let maxX = paper |> List.map fst |> List.max
    let maxY = paper |> List.map snd |> List.max

    for y in 0..maxY do
        for x in 0..maxX do
            printf "%s" (if List.contains (x,y) paper then "█" else " ")

        printfn ""

let fold paper f =
    paper
    |> List.map (fun (x, y) ->
        match f with
        | FoldX f -> if x < f then (x, y) else (f - (x - f), y)
        | FoldY f -> if y < f then (x, y) else (x, f - (y - f))
        )
    |> List.distinct

let part1 (paper, f::folds) =
    fold paper f
    |> List.length

let part2 (paper, folds) =
    (paper, folds)
    ||> List.fold fold
    |> print


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
