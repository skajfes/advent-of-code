open System.IO
open FParsec

let pDirection = choice [pchar 'R'; pchar 'L'; pchar 'U'; pchar 'D']
let pColor = pchar '#' >>. many1Chars hex |>> fun color -> color[0..4], color[5]

let to_dir =
    function
    | '0' -> 'R'
    | '1' -> 'D'
    | '2' -> 'L'
    | '3' -> 'U'
    | _ -> failwith "nope"

let to_len i =
    System.Convert.ToInt64(i, 16)

let pLine = pDirection .>> spaces .>>. pint64 .>> spaces .>>. between (pchar '(') (pchar ')') pColor
            |>> fun ((dir, len), (len2, dir2)) -> dir, len, to_dir dir2, to_len len2

let parse (input: string[]) =
    input
    |> Array.map (fun line ->
        match run pLine line with
        | Success (r, _, _) -> r
        | Failure (a, _, _) -> failwithf $"failed %A{a}")

let to_coordinates dirs =
    ([(0L, 0L)], dirs)
    ||> Array.fold (fun ((x, y)::coords) dir ->
        let next =
            match dir with
            | 'R', len -> x + len, y
            | 'L', len -> x - len, y
            | 'U', len -> x, y + len
            | 'D', len -> x, y - len
            | _ -> failwith "not a coord"

        next::(x, y)::coords)
    |> List.rev

let to_area coords =
    // shoelace formula to calculate area
    coords
    |> List.pairwise
    |> List.map (fun ((x1, y1), (x2, y2)) -> x1*y2 - y1*x2)
    |> List.sum
    |> abs
    |> fun x -> x / 2L

let count_boundary coords =
    // counts the numbers of integer points on boundary
    coords
    |> List.pairwise
    |> List.map (fun ((x1, y1), (x2, y2)) ->
        abs (x2 - x1) + abs (y2 - y1)
        )
    |> List.sum

let get_interior area boundary =
    // uses Pick's theorem formula to get interior points from area and boundary
    area + 1L - (boundary/2L)

let total_area coords =
    let area = to_area coords
    let boundary = count_boundary coords
    let interior = get_interior area boundary
    boundary + interior

let part1 dirs =
    dirs
    |> Array.map (fun (a, b, c, d) -> a, b)
    |> to_coordinates
    |> total_area

let part2 dirs =
    dirs
    |> Array.map (fun (a, b, c, d) -> c, d)
    |> to_coordinates
    |> total_area

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
