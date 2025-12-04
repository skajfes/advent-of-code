open System.IO
open FSharp.HashCollections

let parse (input: string[]) =
    input
    |> Array.mapi (fun i s -> s |> Seq.mapi (fun j c -> (i, j), c) |> Seq.toArray)
    |> Array.concat
    |> Array.filter (snd >> (=) '@')
    |> Array.map fst
    |> HashSet.ofSeq

let neighbours (i,j) =
        [|(i-1, j-1); (i-1, j); (i-1, j+1)
          (i, j-1);             (i, j+1)
          (i+1, j-1); (i+1, j); (i+1, j+1)|]

let remove_rolls (rolls: HashSet<int*int>) =
    let removed =
        rolls
        |> Seq.filter (fun r ->
            neighbours r
            |> Array.map (fun e -> HashSet.contains e rolls)
            |> Array.filter ((=) true)
            |> Array.length
            |> (>) 4
        )
        |> HashSet.ofSeq

    (Seq.length removed, HashSet.difference rolls removed |> HashSet.ofSeq)

let part1 rolls =
    remove_rolls rolls |> fst

let part2 rolls =
    // remove rolls until none are removed
    Seq.unfold (fun rolls ->
        match remove_rolls rolls with
        | 0, _ -> None
        | cnt', rolls' -> Some (cnt', rolls')
        ) rolls
    |> Seq.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
