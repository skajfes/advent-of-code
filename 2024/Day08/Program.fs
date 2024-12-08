open System.IO

let parse (input: string[]) = 
    Array.length input,
    input
    |> Array.mapi (fun r row ->
        row
        |> Seq.mapi (fun c antenna -> (r, c), antenna))
    |> Seq.concat
    |> Seq.filter (snd >> (<>) '.')
    |> Seq.toList

let rec all_pairs antennas =
    match antennas with
    | [] -> []
    | [_] -> []
    | a1::a2::antennas ->
        List.append ((a1, a2)::all_pairs (a1::antennas)) (all_pairs (a2::antennas))
        |> List.distinct

let first_antinodes ((ar, ac), (br, bc)) =
    [
        (ar - (br-ar), ac - (bc-ac))
        (br + (br-ar), bc + (bc-ac))
    ]

let all_antinodes ((ar, ac), (br, bc)) =
    [ for i in -50..50 -> (ar + i * (br-ar), ac + i * (bc - ac))]

let count_antinodes find_nodes (n, antennas) =
    antennas
    |> List.groupBy snd
    |> List.map (fun (_, antennas) -> antennas |> List.map fst)
    |> List.map all_pairs
    |> List.map (fun pairs -> pairs
                              |> List.map find_nodes
                              |> List.concat
                              |> List.filter (fun (r,c) -> r>= 0 && r < n && c >= 0 && c<n )
                              |> List.distinct
                              )
    |> List.concat
    |> List.distinct
    |> List.length

let part1 (n, antennas) =
    count_antinodes first_antinodes (n, antennas)

let part2 (n, antennas) =
    count_antinodes all_antinodes (n, antennas)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
