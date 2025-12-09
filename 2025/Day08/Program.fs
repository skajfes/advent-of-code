open System
open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun s -> s.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Array.map int64)
    |> Array.map (fun [| x;y;z |] -> x, y, z)

let dist (xa, ya, za) (xb, yb, zb) =
    (pown (int64 xb - xa) 2) + (pown (yb - ya) 2) + (pown (zb - za) 2)

let connect_nodes n coords =
    let lengths =
        coords
        |> Array.mapi (fun i a -> coords[i..]
                               |> Array.filter ((<>)a)
                               |> Array.map (fun b -> dist a b, a, b))
        |> Array.concat
        |> Array.sort // by length as its first field
        |> Array.map (fun (_, b, c) -> b, c) // discard the length
        |> fun x ->
            match n with
            | Some n -> Array.take n x // take first n for part 1
            | None -> x // take all for part 2

    // initialize all circuits as containing only one node
    let circuits = coords |> Array.map (fun c -> HashSet([c])) |> Array.toList

    // find node that connects all circuits
    ((None, circuits), lengths)
    ||> Array.fold (fun (res, circuits: HashSet<int64*int64*int64> list) (a, b) ->

        // if node found do nothing
        match res with
        | Some _ -> res, circuits
        | None ->

        // any circuit containing node a
        let with_a = circuits |> List.filter _.Contains(a) |> List.tryHead
        // any circuit containing node b
        let with_b = circuits |> List.filter _.Contains(b) |> List.tryHead

        match with_a, with_b with
        | Some ca, Some cb when ca = cb ->
            // in same circuit - skip
            None, circuits
        | Some ca, Some cb ->
            // if both -> join circuits into one
            cb |> Seq.iter (fun n -> ca.Add(n) |> ignore)
            cb.Clear()

            // remove empty circuits
            let circuits = circuits |> List.filter (fun c -> c.Count > 0)

            // if last circuit then set the resulting node
            if List.length circuits = 1 then
                (Some (a, b)), circuits
            else
                None, circuits
        | _, _ ->
            failwith "should not happen"
        )

let part1 n coords =
    connect_nodes (Some n) coords
    |> snd
    |> Seq.toList
    |> List.map Seq.length
    |> List.sortDescending
    |> List.take 3
    |> List.fold (fun a b -> a * (int64 b)) 1L

let part2 coords =
    connect_nodes None coords
    |> function
        | Some ((xa, _, _), (xb, _, _)), _ -> xa * xb
        | _ -> failwith "nope"

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 10 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 1000 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
