open System.Collections.Generic
open System.Diagnostics
open System.IO

let parse (input: string[]) = 
    input
    |> Array.mapi (fun r row ->
        row |> Seq.mapi (fun c cell -> (r, c), cell))
    |> Seq.concat
    |> Seq.fold (fun (start, stop, path) (c, cell)->
        match cell with
        | '.' -> start, stop, Set.add c path
        | 'S' -> c, stop, Set.add c path
        | 'E' -> start, c, Set.add c path
        | _ -> start, stop, path
        ) ((0, 0), (0, 0), Set.empty)

type Direction = Up | Down | Left | Right

let neighbours (r, c) dir =
    match dir with
    | Up -> [
        1, (r - 1, c), Up
        1000, (r, c), Right
        1000, (r, c), Left
        ]
    | Down -> [
        1, (r + 1, c), Down
        1000, (r, c), Right
        1000, (r, c), Left
        ]
    | Right -> [
        1, (r, c + 1), Right
        1000, (r, c), Up
        1000, (r, c), Down
        ]
    | Left -> [
        1, (r, c - 1), Left
        1000, (r, c), Up
        1000, (r, c), Down
        ]

let opposite =
    function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let walk ((start, start_dir), (stop, stop_dir), path) =

    let mutable map = Dictionary<_ ,_>()
    let visited = HashSet<(int * int) * Direction>()

    let rec do_walk (queue: PriorityQueue<int * (int * int) * Direction, int>) =
        let steps, pos, dir = queue.Dequeue()

        if pos = stop && (stop_dir = None || stop_dir = Some dir) then
            map.Add((pos, dir), steps)
            steps, map
        else

        if visited.Contains(pos, dir) then do_walk queue else
        visited.Add(pos, dir) |> ignore
        map.Add((pos, dir), steps)

        neighbours pos dir
        |> List.filter (fun (s, p, d) -> Set.contains p path)
        |> List.iter (fun (s, (r, c), d) ->
            queue.Enqueue((steps + s, (r, c), d), steps + s))
        do_walk queue

    let queue = PriorityQueue<int * (int * int) * Direction, int>()
    queue.Enqueue((0, start, start_dir), 0)
    do_walk queue


let part1 (start, stop, path) =
    walk ((start, Right), (stop, None), path) |> fst

let part2 (start, stop, path) =
    // walk from start to stop
    let steps, dist = walk ((start, Right), (stop, None), path)
    // walk from stop to start, take optimal stop direction
    let stop_dir = dist.Keys |> Seq.filter (fun ((r, c), d) -> (r, c) = stop) |> Seq.minBy (fun k -> dist[k]) |> snd
    let _, dist_end = walk ((stop, opposite stop_dir), (start, Some Right), path)

    // find all points that are on the optimal path (steps from start + steps from stop = min steps)
    dist_end
    |> Seq.fold (fun res kv ->
        let p, d = kv.Key
        if kv.Value + dist.GetValueOrDefault((p, opposite d)) = steps then res |> Set.add p else res) Set.empty
    |> Set.count


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
