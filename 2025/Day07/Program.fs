open System.Collections.Generic
open System.IO

let parse (input: string[]) =
    let data =
        input
        |> Array.mapi (fun r row -> row |> Seq.mapi (fun c col -> (r, c), col) |> Seq.toArray)
        |> Array.concat
        |> Array.filter (snd >> (<>)'.')
    let start = data |> Array.filter (snd >> (=)'S') |> Array.map fst |> Array.head
    let manifolds = data |> Array.filter (snd >> (<>) 'S') |> Array.map fst
    start, manifolds

let part1 (start: int*int, manifolds:(int*int)array) =
    let visited = HashSet<int*int>()
    let rec split (r, c) =
        if visited.Contains((r, c)) then [] else
        visited.Add(r, c) |> ignore
        let m = manifolds
                |> Array.filter (snd >> (=)c)
                |> Array.filter (fst >> (<)r)
                |> Array.tryHead
        match m with
        | Some (rm, cm) ->
            let left = split (rm, cm - 1)
            let right = split (rm, cm + 1)
            (rm, cm)::left @ right
        | None ->
            []
    split start
    |> List.distinct
    |> List.length

let part2 (start: int*int, manifolds:(int*int)array) =
    let visited = Dictionary<int*int, int64>()
    let rec split (r, c) =
        if visited.ContainsKey((r, c)) then visited[r, c] else
        let m = manifolds
                |> Array.filter (snd >> (=)c)
                |> Array.filter (fst >> (<)r)
                |> Array.tryHead
        match m with
        | Some (rm, cm) ->
            let left = split (rm, cm - 1)
            let right = split (rm, cm + 1)
            visited.Add((r, c), left + right) |> ignore
            left + right
        | None ->
            visited.Add((r, c), 1) |> ignore
            1L
    split start

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
