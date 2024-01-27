open System
open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun line -> line.Split([|':'; ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
    |> Array.map (fun (n::ns) -> ns |> List.map (fun node -> [| n, node; |]) |> Array.ofList |> Array.concat)
    |> Array.concat

// let numberOfPaths nodes =
//
//     let start = nodes |> Map.keys |> Seq.sortBy (fun x -> Random.Shared.Next()) |> Seq.head
//
//     let visited = HashSet<string>()
//     let rec find todo paths =
//         if todo = [] then
//             printfn "%d" visited.Count
//             paths else
//
//         let node::todo = todo
//
//         if visited.Contains(node) then find todo paths else
//
//         visited.Add(node) |> ignore
//
//         let ns =
//             nodes
//             |> Map.find node
//             // |> Array.filter (fun x -> visited.Contains(x) |> not)
//         let paths =
//             ns
//             |> Array.fold (fun paths node ->
//                 match paths |> Map.tryFind node with
//                 | None -> paths |> Map.add node 1
//                 | Some l -> paths |> Map.add node (l + 1)
//                 ) paths
//
//         find (todo @ (ns|>List.ofArray)) paths
//
//     find [start] Map.empty

let rec contract nodes edges =
    // printfn "%A\n%A" nodes (edges)
    // Console.ReadKey() |> ignore
    match Array.length nodes with
    | 2 ->
        let [|a; b|] = nodes
        let res = Array.length edges, (String.length a) / 3, (String.length b) / 3
        printfn "%A" res
        res
    | n when edges = Array.empty -> 0, 8888, 8888
    | n ->
        // printfn "%d %d" (n) (edges.Length)
        // let a, b = edges |> Array.countBy id |> Array.maxBy snd |> fst
        // let a, b = edges[Random.Shared.Next(edges.Length)]
        let groups = edges |> Array.countBy id
        let max = groups |> Array.map snd |> Array.max
        let maxGroup = groups |> Array.filter (snd >> (=)max) |> Array.map fst
        let a, b = maxGroup[Random.Shared.Next(maxGroup.Length)]
        // let edges = Array.tail edges

        // replace node
        let nodes =
            nodes
            |> Array.filter (fun x -> x <> a && x <> b)
            |> Array.append [| a+b |]

        let edges =
            edges
            |> Array.filter (fun (a', b') -> (a, b) <> (a', b') && (a, b) <> (b', a'))
            |> Array.map (function
                    | a', b' when a = a' -> a+b, b'
                    | a', b' when a = b' -> a+b, a'
                    | a', b' when b = a' -> a+b, b'
                    | a', b' when b = b' -> a+b, a'
                    | a', b' -> a', b'
                    )

        contract nodes edges




let part1 edges =
    // nodes
    // |> Array.groupBy fst
    // |> Array.map (fun (a, b) -> a, b |> Array.map snd)
    // |> Map.ofArray
    // |> numberOfPaths
    // |> Map.iter (printfn "%s: %d")
    // |> Map.toList
    // |> List.groupBy snd
    // |> List.map (fun (a, b) -> a, b |> List.length)
    // |> List.partition (fst >> (>=) 4)
    // |> fun (a, b) ->
    //     (a |> List.length) , (b |> List.length), (a |> List.length) * (b |> List.length)
    let nodes =
        edges
        |> Array.collect (fun (a, b) -> [| a; b |])
        |> Array.distinct
    [1..100]
    |> List.map (fun _ -> contract nodes edges)
    |> List.min

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
