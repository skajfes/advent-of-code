open System
open System.Collections.Generic
open System.IO

// type Board = (int * char) list
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


// let maze1 =
//     [(0, 0), "0"; (0, 1), "1"; (0, 2), "-1"; (0, 3), "2"; (0, 4), "-2"; (0, 5), "3"; (0, 6), "-3"; (0, 7), "4"; (0, 8), "-4"; (0, 9), "5"; (0, 10), "6";
//                                 (1, 2), "7";              (1, 4), "9";              (1, 6), "11";             (1, 8), "13";
//                                 (2, 2), "8";              (2, 4), "10";             (2, 6), "12";             (2, 8), "14"; ]

// let maze2 =
//     [(0, 0), "0"; (0, 1), "1"; (0, 2), "-1"; (0, 3), "2"; (0, 4), "-2"; (0, 5), "3"; (0, 6), "-3"; (0, 7), "4"; (0, 8), "-4"; (0, 9), "5"; (0, 10), "6";
//                                 (1, 2), "7";              (1, 4), "9";              (1, 6), "11";             (1, 8), "13";
//                                 (2, 2), "8";              (2, 4), "10";             (2, 6), "12";             (2, 8), "14";
//                                 (3, 2), "15";             (3, 4), "16";             (3, 6), "17";             (3, 8), "18";
//                                 (4, 2), "19";             (4, 4), "20";             (4, 6), "21";             (4, 8), "22" ]
let maze1 =
    [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7); (0, 8); (0, 9); (0, 10);
                     (1, 2);         (1, 4);         (1, 6);         (1, 8);
                     (2, 2);         (2, 4);         (2, 6);         (2, 8); ]
                     // (3, 2);         (3, 4);         (3, 6);         (3, 8);
                     // (4, 2);         (4, 4);         (4, 6);         (4, 8); ]

let maze2 = List.append maze1 [
                      (3, 2);         (3, 4);         (3, 6);         (3, 8);
                      (4, 2);         (4, 4);         (4, 6);         (4, 8); ]

let firstRow = [(0, 0); (0, 1); (0, 3); (0, 5); (0, 7); (0, 9); (0, 10)]

// let pathCache = Dictionary<int*int, int list>()
// let getPath maze start stop =
//
//     let getPathImpl start stop =
//         let mm p =
//             maze |> List.filter (fst>>(=)p) |> List.head |> snd
//
//         let s = maze |> List.filter (snd >> (=)(string start)) |> List.head
//         let e = maze |> List.filter (snd >> (=)(string stop)) |> List.head
//         let xs = (fst >> fst) s
//         let xe = (fst >> fst) e
//         let ys = (fst >> snd) s
//         let ye = (fst >> snd) e
//
//         if xs = xe then
//             if ys = ye then []
//             else [ys .. (if ye > ys then 1 else -1) .. ye] |> List.map (fun x -> 0, x)
//         else
//             let col = [ys .. (if ye > ys then 1 else -1) .. ye] |> List.map (fun x -> 0, x)
//             let row = [xs .. (if xe > xs then 1 else -1) .. xe] |> List.map (fun x -> x, if xe > 0 then ye else ys)
//             row @ col
//
//         |> List.map mm
//         |> List.map int
//         |> List.filter ((<>) start)
//         |> List.distinct
//
//     if pathCache.ContainsKey(start, stop)
//     then pathCache[start, stop]
//     else
//         let path = getPathImpl start stop
//         pathCache.Add((start, stop), path)
//         path

let pathCache' = Dictionary<(int*int)*(int*int), (int*int) list>()
let getPath' start stop =

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

    if pathCache'.ContainsKey(start, stop)
    then pathCache'[start, stop]
    else
        let path = getPathImpl start stop
        pathCache'.Add((start, stop), path)
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

let allPossiblePositions (board: Board) struct (start, amphipod) : ((Board * int) list) =
    let has (key, value) = board |> FSharp.HashCollections.HashMap.containsKey key && board.Item(key) = value
    match start with
    | x, y when x = 0 ->
        match amphipod with
        | 'A' when has ((2, 2), 'A') -> [(1, 2)]
        | 'A' -> [(2, 2)]
        | 'B' when has ((2, 4), 'B') -> [(1, 4)]
        | 'B' -> [(2, 4)]
        | 'C' when has ((2, 6), 'C') -> [(1, 6)]
        | 'C' -> [(2, 6)]
        | 'D' when has ((2, 8), 'D') -> [(1, 8)]
        | 'D' -> [(2, 8)]
        | _ -> failwith "unknown amphipod"
    | x, y when x > 0 ->
        match amphipod, start with
        | 'A', (2, 2) -> []
        | 'A', (1, 2) when has ((2, 2), 'A') -> []
        | 'A', _ -> firstRow
        | 'B', (2, 4) -> []
        | 'B', (1, 4) when has ((2, 4), 'B') -> []
        | 'B', _ -> firstRow
        | 'C', (2, 6) -> []
        | 'C', (1, 6) when has ((2, 6), 'C') -> []
        | 'C', _ -> firstRow
        | 'D', (2, 8) -> []
        | 'D', (1, 8) when has ((2, 8), 'D') -> []
        | 'D', _ -> firstRow
        | _ -> failwith "no"
    | _ -> failwith "unknown position"
    |> List.map (fun stop -> stop, getPath' start stop)
    |> List.filter (fun (stop, path) -> pathEmpty board path)
    |> List.map (fun (pos, path) -> (board
                                     |> FSharp.HashCollections.HashMap.remove start
                                     |> FSharp.HashCollections.HashMap.add pos amphipod), score amphipod path)

let nextPositions maze (board: Board): ((Board*int) list) =
    board
    |> FSharp.HashCollections.HashMap.toSeq
    |> Seq.toList
    |> List.collect (allPossiblePositions board)

// let goal1 = [|(7, 'A'); (8, 'A'); (9, 'B'); (10, 'B'); (11, 'C'); (12, 'C'); (13, 'D'); (14, 'D'); |]
// let goal2 = Array.append goal1 [|(7, 'A'); (8, 'A'); (9, 'B'); (10, 'B'); (11, 'C'); (12, 'C'); (13, 'D'); (14, 'D'); |]
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

let shortestPath isDone (maze: (int*int)list) (board: Board) =
    let visited = HashSet<Board>()
    let queue = PriorityQueue<int*Board, int>()
    queue.Enqueue((0, board), 0)
    let mutable lastCost = 0

    let rec find () =
        let (cost, board) = queue.Dequeue()

        if visited.Contains(board) then find() else

        // if cost > lastCost then
        //     printfn "%d" cost
        //     lastCost <- cost

        if isDone board then cost else
        visited.Add(board) |> ignore

        nextPositions maze board
        |> List.filter (fun (board, cost) -> visited.Contains(board) |> not)
        |> List.map (fun (board, c) -> c + cost, board)
        |> List.iter (fun item -> queue.Enqueue(item, fst item) )

        find ()
    find ()

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> printfn "%A"
    // let board = testInput |> parse
    // nextPositions board |> printfn "%A"
    // shortestPath board |> printfn "%A"
    // testInput |> parse |> shortestPath |> printfn "%A"

    // testInput |> parse |> shortestPath (isDone goal1) maze1 |> printfn "Test 1: %A"
    // input |> parse |> shortestPath (isDone goal1) maze1 |> printfn "Part 1: %A"
    testInput |> parse |> shortestPath (isDone goal2) maze2 |> printfn "Test 2: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
