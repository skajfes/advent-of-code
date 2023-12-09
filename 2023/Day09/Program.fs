open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun x -> x.Split(' ') |> Array.map int)


let diff xs =
    xs
    |> Array.pairwise
    |> Array.map (fun (a, b) -> b - a)

let rec diffs prev acc xs =
    if Array.forall ((=)0) xs then 0 else
    acc (prev xs) (diffs prev acc (diff xs))

let part1 = Array.sumBy (diffs Array.last (+))
let part2 = Array.sumBy (diffs Array.head (-))

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
