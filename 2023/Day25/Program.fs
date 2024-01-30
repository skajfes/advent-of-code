open System
open System.Diagnostics
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun line -> line.Split([|':'; ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
    |> Array.map (fun (n::ns) -> ns |> List.map (fun node -> [| n, node; |]) |> Array.ofList |> Array.concat)
    |> Array.concat

let rec contract (nodes: Map<string, int>) (edges: (string * string) array) nodes_count =
    match nodes_count with
    | 2 ->
        // printfn "%A" (nodes.Values )
        let [|a; b|] = nodes.Values |> Seq.toArray
        let res = (a, b), Array.length edges
        printfn "%A" res
        res
    | _ ->
        // pick random edge
        let a, b = edges[Random.Shared.Next(edges.Length)]

        // replace node
        let cnt = nodes[a] + nodes[b]
        let nodes =
            nodes
            |> Map.add a cnt
            |> Map.remove b

        let edges =
            edges
            |> Array.filter (fun (a', b') -> (a <> a' || b <> b') && (a <> b' || b <> a'))
            |> Array.map (function
                    | a', b' when a = a' -> a, b'
                    | a', b' when a = b' -> a, a'
                    | a', b' when b = a' -> a, b'
                    | a', b' when b = b' -> a, a'
                    | a', b' -> a', b'
                    )

        contract nodes edges (nodes_count - 1)

let get_nodes edges =
    edges
    |> Array.collect (fun (a, b) -> [| a; b |])
    |> Array.distinct
    |> Array.map (fun x -> x, 1)
    |> Map.ofArray

let part1 (edges: (string*string) array) =

    let ns = get_nodes edges
    // try until a min cut of 3 is found
    Seq.unfold (fun cut -> if cut=3 then None else contract ns edges (ns.Count) |> Some) 0
    |> Seq.last
    |> fun (a, b) -> a * b

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"

    let timer = Stopwatch.StartNew()
    input |> parse |> part1 |> printfn "Part 1: %A"
    timer.Stop()
    printfn "Executed in %A" timer.Elapsed

    0 // return an integer exit code
