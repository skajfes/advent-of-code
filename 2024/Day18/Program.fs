open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun x -> x.Split(",")
                           |> fun [| x; y |] -> (int x, int y))

let fall n t bytes =
    let fallen = bytes |> Array.take t |> Set.ofArray

    [for x in 0..n-1 do for y in 0..n-1 -> x, y]
    |> Set.ofList
    |> fun s -> Set.difference s fallen

let neighbours (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let shortest_path n path =
    let start = (0, 0)
    let stop = (n - 1, n - 1)

    let visited = HashSet<int * int>()

    let rec do_walk (queue: PriorityQueue<int * (int * int) * (int * int) list, int>) =
        if queue.Count = 0 then None else

        let steps, pos, shortest = queue.Dequeue()
        if pos = stop then Some (steps, shortest) else

        if visited.Contains(pos) then do_walk queue else
        visited.Add(pos) |> ignore

        neighbours pos
        |> List.filter (fun p -> path |> Set.contains p)
        |> List.iter (fun p -> queue.Enqueue((steps + 1, p, p::shortest), steps + 1))

        do_walk queue

    let queue = PriorityQueue<int * (int * int) * (int*int)list, int>()
    queue.Enqueue((0, start, []), 0)

    do_walk queue


let find_blocker n skip bytes =
    bytes
    |> Seq.skip skip
    |> Seq.mapFold (fun memory b ->
        let mem = memory |> Set.remove b
        (b, mem), mem) (fall n skip bytes)
    |> fst
    |> Seq.fold (fun (res, shortest) (b, memory) ->
        match res, shortest with
        | Some _, _ -> res, shortest
        | None, _ when List.contains b shortest || shortest = [] ->
            // only change shortest path when bytes directly fall on it
            match shortest_path n memory with
            | None -> Some b, []
            | Some (_, path) -> None, path
        | _ -> None, shortest
    ) (None, [])
    |> fst

let part1 grid_size falling_time bytes =
    bytes
    |> fall grid_size falling_time
    |> shortest_path grid_size
    |> function
        | Some (min_length, _) -> min_length
        | None -> failwith "path not found"

let part2 grid_size skip bytes =
    find_blocker grid_size skip bytes
    |> function
        | Some (x, y) -> sprintf "%d,%d" x y
        | None -> failwith "Not found"

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 7 12 |> printfn "%A"
    testInput |> parse |> part2 7 12 |> printfn "%A"

    input |> parse |> part1 71 1024 |> printfn "Part 1: %d"
    input |> parse |> part2 71 1024 |> printfn "Part 2: %s"

    0 // return an integer exit code
