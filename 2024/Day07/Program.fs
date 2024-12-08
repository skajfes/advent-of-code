open System.IO
open FParsec

let pline = pint64 .>> pstring ": " .>>. sepBy1 pint64 spaces1

let line l =
    match run pline l with
    | Success ((res, nums), _, _) -> res, nums
    | Failure (s, e, _) -> failwith s

let parse (input: string[]) = 
    input
    |> Array.map line

let rec eq_try enable_concat res so_far nums =
    if so_far > res then false else
        match nums with
        | [] -> res = so_far
        | n1::ns ->
            eq_try enable_concat res (so_far + n1) ns ||
            eq_try enable_concat res (so_far * n1) ns ||
            (enable_concat && eq_try enable_concat res (string so_far + string n1 |> int64) ns)

let part1 input =
    input
    |> Array.filter (fun (res, n::ns) -> eq_try false res n ns)
    |> Array.sumBy fst

let part2 input =
    input
    |> Array.filter (fun (res, n::ns) -> eq_try true res n ns)
    |> Array.sumBy fst

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
