open System.Collections.Generic
open System.IO

type Board = FSharp.HashCollections.HashMap<int*int, char>

let parse (input: string[]) =
    [
        ((1, 2), input[2].[3])
        ((2, 2), input[3].[3])
        ((1, 4), input[2].[5])
        ((2, 4), input[3].[5])
        ((1, 6), input[2].[7])
        ((2, 6), input[3].[7])
        ((1, 8), input[2].[9])
        ((2, 8), input[3].[9])
    ]
    |> List.map (fun (k, v) -> KeyValuePair(k, v))
    |> FSharp.HashCollections.HashMap.ofSeq

// parse with added rows for part2
let parse2 (input: string[]) =
    [
        ((1, 2), input[2].[3])
        ((2, 2), 'D')
        ((3, 2), 'D')
        ((4, 2), input[3].[3])
        ((1, 4), input[2].[5])
        ((2, 4), 'C')
        ((3, 4), 'B')
        ((4, 4), input[3].[5])
        ((1, 6), input[2].[7])
        ((2, 6), 'B')
        ((3, 6), 'A')
        ((4, 6), input[3].[7])
        ((1, 8), input[2].[9])
        ((2, 8), 'A')
        ((3, 8), 'C')
        ((4, 8), input[3].[9])
    ]
    |> List.map (fun (k, v) -> KeyValuePair(k, v))
    |> FSharp.HashCollections.HashMap.ofSeq

// determine path between two coordinates
let pathCache = Dictionary<(int*int)*(int*int), (int*int) list>()
let getPath start stop =

    let getPathImpl (xs, ys) (xe, ye) =
        if xs = xe then
            if ys = ye then []
            else [ys .. (if ye > ys then 1 else -1) .. ye] |> List.map (fun x -> 0, x)
        else
            let col = [ys .. (if ye > ys then 1 else -1) .. ye] |> List.map (fun x -> 0, x)
            let row = [xs .. (if xe > xs then 1 else -1) .. xe] |> List.map (fun x -> x, if xe > 0 then ye else ys)
            row @ col

        |> List.filter ((<>) start)
        |> List.distinct

    if pathCache.ContainsKey(start, stop)
    then pathCache[start, stop]
    else
        let path = getPathImpl start stop
        pathCache.Add((start, stop), path)
        path

let pathEmpty (board: Board) path =
    path |> List.forall (fun pos -> board |> FSharp.HashCollections.HashMap.containsKey pos |> not)

let score amphipod path =
    let multiplier =
        match amphipod with
        | 'A' -> 1
        | 'B' -> 10
        | 'C' -> 100
        | 'D' -> 1000
        | _ -> failwith "unknown amphipod"
    path
    |> List.length
    |> (fun x -> x * multiplier)

let allPossiblePositions' moveToRoom (board: Board) struct (start, amphipod) : (Board * int) list =
    match start with
    | x, _ when x = 0 ->
        match amphipod with
        | 'A' -> moveToRoom 'A' 2
        | 'B' -> moveToRoom 'B' 4
        | 'C' -> moveToRoom 'C' 6
        | 'D' -> moveToRoom 'D' 8
        | _ -> failwith "unknown amphipod"
    | x, _ when x > 0 -> [(0, 0); (0, 1); (0, 3); (0, 5); (0, 7); (0, 9); (0, 10)] // first row
    | _ -> failwith "unknown position"
    |> List.map (fun stop -> stop, getPath start stop)
    |> List.filter (fun (_, path) -> pathEmpty board path)
    |> List.map (fun (pos, path) -> (board
                                     |> FSharp.HashCollections.HashMap.remove start
                                     |> FSharp.HashCollections.HashMap.add pos amphipod), score amphipod path)

let allPossiblePositions (board: Board) struct (start, amphipod) : (Board * int) list =
    let has (key, value) = board |> FSharp.HashCollections.HashMap.containsKey key && board.Item(key) = value
    let moveToRoom a c =
        if has ((2, c), a) |> not then [(2, c)]
        elif has ((1, c), a) |> not then [(1, c)]
        else [(2, c)]

    allPossiblePositions' moveToRoom board (start, amphipod)

let allPossiblePositions2 (board: Board) struct (start, amphipod) : (Board * int) list =
    let has (key, value) = board |> FSharp.HashCollections.HashMap.containsKey key && board.Item(key) = value
    let moveToRoom a c =
        if has ((4, c), a) |> not then [(4, c)]
        elif has ((3, c), a) |> not then [(3, c)]
        elif has ((2, c), a) |> not then [(2, c)]
        elif has ((1, c), a) |> not then [(1, c)]
        else [(4, c)]

    allPossiblePositions' moveToRoom board (start, amphipod)

let nextPositions allPossiblePositions (board: Board): (Board*int) list =
    board
    |> FSharp.HashCollections.HashMap.toSeq
    |> Seq.toList
    |> List.collect (allPossiblePositions board)

let goal1 =
    [| ((1, 2), 'A')
       ((2, 2), 'A')
       ((1, 4), 'B')
       ((2, 4), 'B')
       ((1, 6), 'C')
       ((2, 6), 'C')
       ((1, 8), 'D')
       ((2, 8), 'D') |]
let goal2 =
    Array.append goal1 [|
       ((3, 2), 'A')
       ((4, 2), 'A')
       ((3, 4), 'B')
       ((4, 4), 'B')
       ((3, 6), 'C')
       ((4, 6), 'C')
       ((3, 8), 'D')
       ((4, 8), 'D') |]

let isDone goal (board: Board) =
    goal
    |> Array.forall (fun (k, v) -> board |> FSharp.HashCollections.HashMap.containsKey k && board.Item(k) = v)

// dijkstra for shortest path
let shortestPath isDone allPossiblePositions (board: Board) =
    let visited = HashSet<Board>()
    let queue = PriorityQueue<int*Board, int>()
    queue.Enqueue((0, board), 0)
    let mutable lastCost = 0

    let rec find () =
        let cost, board = queue.Dequeue()

        if visited.Contains(board) then find() else

        // if cost > lastCost then
        //     printfn "%d %d" (queue.Count) cost
        //     lastCost <- cost

        if isDone board then cost else
        visited.Add(board) |> ignore

        nextPositions allPossiblePositions board
        |> List.filter (fun (board, _) -> visited.Contains(board) |> not)
        |> List.map (fun (board, c) -> c + cost, board)
        |> List.iter (fun item -> queue.Enqueue(item, fst item) )

        find ()
    find ()

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    // testInput |> parse |> shortestPath (isDone goal1) allPossiblePositions |> printfn "Test 1: %A"
    // testInput |> parse2 |> shortestPath (isDone goal2) allPossiblePositions2 |> printfn "Test 2: %A"

    input |> parse |> shortestPath (isDone goal1) allPossiblePositions |> printfn "Part 1: %A"
    input |> parse2 |> shortestPath  (isDone goal2) allPossiblePositions2 |> printfn "Part2 2: %A"

    0 // return an integer exit code
