open System
open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.mapi (fun r row ->
        row |> Seq.mapi (fun c cell -> (r, c), cell))
    |> Seq.concat
    |> Seq.fold (fun (start, stop, path) (c, cell)->
        match cell with
        | '.' -> start, stop, c::path
        | 'S' -> c, stop, c::path
        | 'E' -> start, c, c::path
        | _ -> start, stop, path
        ) ((0, 0), (0, 0), [])

type Direction = Up | Down | Left | Right

let ns (r, c) dir =
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

let walk (start, stop, path) =

    let visited = HashSet<(int * int) * Direction>()
    let map = Dictionary<(int * int) * Direction, int*(int*int)list>()
    map.Add((start, Right), (0, []))

    let rec do_walk (queue: PriorityQueue<int * (int * int) * Direction * (int*int*Direction) list, int>) =
        let steps, pos, dir, tiles = queue.Dequeue()

        if map.ContainsKey(pos, dir) then
            let s, p = map[pos, dir]
            if s = steps then
                map.Remove((pos, dir)) |> ignore
                map.Add((pos, dir), (s, p |> List.append (tiles |> List.map (fun (a, b, c) -> a, b))))
        else
            map.Add((pos, dir), (steps, tiles |> List.map (fun (a, b, c) -> a, b)))

        if pos = stop then
            tiles
            |> List.collect (fun (r, c, d) -> map[(r, c), d] |> snd)
            |> List.distinct
            |> List.length
        else


        if visited.Contains(pos, dir) then do_walk queue else
        visited.Add(pos, dir) |> ignore

        let ns' =
            ns pos dir
            |> List.filter (fun (s, p, d) -> path |> List.contains p)

        printfn "In %d %A %A %A" steps pos dir ns'
        // Console.ReadKey() |> ignore

        ns'
        |> List.iter (fun (s, (r, c), d) ->
            queue.Enqueue((steps + s, (r, c), d, (r, c, d)::tiles), steps + s))
        do_walk queue

    let queue = PriorityQueue<int * (int * int) * Direction * (int * int * Direction) list, int>()
    queue.Enqueue((0, start, Right, []), 0)
    do_walk queue


let part1 (start, stop, path) =
    walk (start, stop, path)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
