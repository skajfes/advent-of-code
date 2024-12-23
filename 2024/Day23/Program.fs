open System.ComponentModel
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun l -> l.Split('-') |> fun [| a; b |] -> a, b)

let find_groups connections =
    let all_connections =
        connections
        |> Array.append (connections |> Array.map (fun (a, b) -> b, a))

    let cons_map =
        all_connections
        |> Array.groupBy fst
        |> Array.map (fun (k, v) -> k, v |> Array.map snd)
        |> Map.ofArray

    cons_map
    |> Map.map (fun a cons ->
        cons |> Array.map (fun b ->
            Map.find b cons_map
            |> Array.filter (fun c -> Array.contains c cons)
            |> Array.map (fun c -> [a; b; c] |> List.sort))
        )
    |> Map.toArray
    |> Array.filter (fun (k: string, v) -> k.StartsWith("t"))
    |> Array.map snd
    |> Array.concat
    |> Array.concat
    |> Array.distinct

let find_groups2 connections =
    let all_connections =
        connections
        |> Array.append (connections |> Array.map (fun (a, b) -> b, a))

    let cons_map =
        all_connections
        |> Array.groupBy fst
        |> Array.map (fun (k, v) -> k, v |> Array.map snd |> Set.ofArray)
        |> Map.ofArray

    // dfs with backtracking to find all maximal cliques
    // for all nodes in potential, add to clique, filter potential by neighbours, filter discarded by neighbours
    // recurse with new clique, potential, discarded
    // if discard and potential is empty we found a valid clique
    // backtrack by removing current node from potential and adding to discarded, continue dfs
    // https://www.youtube.com/watch?v=j_uQChgo72I
    let rec bron_kerbosch clique potential discarded =
        if Set.isEmpty potential && Set.isEmpty discarded then seq { yield clique } else

        let mutable potential = potential
        let mutable discarded = discarded

        seq {
            while not (Set.isEmpty potential) do
                let current = Set.minElement potential

                let neighbours = Map.find current cons_map
                let clique' = Set.add current clique
                let potential' = Set.intersect potential neighbours
                let discarded' = Set.intersect discarded neighbours

                yield! bron_kerbosch clique' potential' discarded'

                potential <- Set.remove current potential
                discarded <- Set.add current discarded
        }


    bron_kerbosch (Set.empty) (Map.keys cons_map |> Set.ofSeq) Set.empty
    |> Seq.sortByDescending Set.count
    |> Seq.map (Set.toList >> List.sort >> String.concat ",")
    |> Seq.head

let part1 connections =
    find_groups connections
    |> Array.length

let part2 connections =
    find_groups2 connections


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    let testInput2 = File.ReadAllLines("sample2.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput2 |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %s"

    0 // return an integer exit code
