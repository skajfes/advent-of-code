open System
open System.Collections.Generic
open System.IO
open FSharpx.Collections

type Board = (int * char) list

let parse (input: string[]) = [
    (7, input[2].[3])
    (8, input[3].[3])
    (9, input[2].[5])
    (10, input[3].[5])
    (11, input[2].[7])
    (12, input[3].[7])
    (13, input[2].[9])
    (14, input[3].[9])
]

let maze' =
    [(0, 0), "0"; (0, 1), "1"; (0, 2), "-1"; (0, 3), "2"; (0, 4), "-2"; (0, 5), "3"; (0, 6), "-3"; (0, 7), "4"; (0, 8), "-4"; (0, 9), "5"; (0, 10), "6";
                                (1, 2), "7";              (1, 4), "9";              (1, 6), "11";             (1, 8), "13";
                                (2, 2), "8";              (2, 4), "10";             (2, 6), "12";             (2, 8), "14"; ]

let pathCache = Dictionary<int*int, int list>()
let getPath start stop =

    let getPathImpl start stop =
        let mm p =
            maze' |> List.filter (fst>>(=)p) |> List.head |> snd

        let s = maze' |> List.filter (snd >> (=)(string start)) |> List.head
        let e = maze' |> List.filter (snd >> (=)(string stop)) |> List.head
        let xs = (fst >> fst) s
        let xe = (fst >> fst) e
        let ys = (fst >> snd) s
        let ye = (fst >> snd) e

        if xs = xe then
            if ys = ye then []
            else [ys .. (if ye > ys then 1 else -1) .. ye] |> List.map (fun x -> 0, x)
        else
            let col = [ys .. (if ye > ys then 1 else -1) .. ye] |> List.map (fun x -> 0, x)
            let row = [xs .. (if xe > xs then 1 else -1) .. xe] |> List.map (fun x -> x, if xe > 0 then ye else ys)
            row @ col

        |> List.map mm
        // |> List.filter ((<>)"_")
        |> List.map int
        |> List.filter ((<>) start)
        |> List.distinct

    if pathCache.ContainsKey(start, stop)
    then pathCache[start, stop]
    else
        let path = getPathImpl start stop
        pathCache.Add((start, stop), path)
        path

// getPath 7 1


let isValidMove' board start stop  =
    getPath start stop
    |> List.forall (fun pos -> board |> List.map fst |> List.contains pos |> not)

let pathEmpty board path =
    path |> List.forall (fun pos -> board |> List.map fst |> List.contains pos |> not)

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

let allPossiblePositions board (start, amphipod) =
    match start with
    | x when x <= 6 ->
        match amphipod with
        | 'A' -> [7; 8]
        | 'B' -> [9; 10]
        | 'C' -> [11; 12]
        | 'D' -> [13; 14]
        | _ -> failwith "unknown amphipod"
    | x when x > 6 ->
        let has item = List.contains item board
        match amphipod, start with
        | 'A', 8 -> []
        | 'A', 7 when has (8, 'A') -> []
        | 'A', _ -> [0..6]
        | 'B', 10 -> []
        | 'B', 9 when has (10, 'B') -> []
        | 'B', _ -> [0..6]
        | 'C', 12 -> []
        | 'C', 11 when has (12, 'C') -> []
        | 'C', _ -> [0..6]
        | 'D', 14 -> []
        | 'D', 13 when has (14, 'D') -> []
        | 'D', _ -> [0..6]
        | _ -> failwith "no"
    | _ -> failwith "unknown position"
    |> List.map (fun stop -> stop, getPath start stop)
    |> List.filter (fun (stop, path) -> pathEmpty board path)
    |> List.map (fun (pos, path) -> ((pos, amphipod) :: (board |> List.filter ((<>)(start, amphipod)))) , score amphipod path)


let nextPositions board =
    board
    |> List.collect (allPossiblePositions board)


let template = [|
    "#############"
    "#...........#"
    "###.#.#.#.###"
    "  #.#.#.#.#  "
    "  #########  "
|]

let toCoord pos =
     match pos with
     | 0 -> 1, 1
     | 1 -> 1, 2
     | 2 -> 1, 4
     | 3 -> 1, 6
     | 4 -> 1, 8
     | 5 -> 1, 10
     | 6 -> 1, 11
     | 7 -> 2, 3
     | 8 -> 3, 3
     | 9 -> 2, 5
     | 10 -> 3, 5
     | 11 -> 2, 7
     | 12 -> 3, 7
     | 13 -> 2, 9
     | 14 -> 3, 9
     | _ -> failwith "nope"

let toString cost board =
    template
    |> Array.mapi (fun r row -> row |> String.mapi (fun c a ->
        match List.tryFind (fun (pos, a) -> toCoord pos = (r, c)) board with
        | Some (_, a) -> a
        | None -> a
        ))
    |> Array.fold (fun o l -> o + "\n" + l) (string cost)
    // |> printfn "%s"



let shortestPath (board: (int*char) list) =
    let goal = [|(7, 'A'); (8, 'A'); (9, 'B'); (10, 'B'); (11, 'C'); (12, 'C'); (13, 'D'); (14, 'D'); |]
    let visited = HashSet<(int * char) list>()
    let queue = PriorityQueue.empty false
                |> PriorityQueue.insert (0, board)
    let isDone board =
        goal
        |> Array.forall (fun x -> List.contains x board)
    let mutable lastCost = 0

    let rec find queue  =
        let (cost, board), queue = PriorityQueue.pop queue

        if visited.Contains(board) then find queue else

        // Console.SetCursorPosition(0, 0)
        // Console.CursorVisible <- false
        // printfn "%s" (toString cost board)
        if cost > lastCost then
            printfn "%d" cost
            lastCost <- cost

        if isDone board then cost else
        visited.Add(board) |> ignore

        let queue =
            nextPositions board
            |> List.filter (fun (board, cost) -> visited.Contains(board) |> not)
            |> List.map (fun (board, c) -> c + cost, board)
            // |> List.map (fun (cost, board) ->
            //     printfn "N: %s" (toString cost board)
            //     cost, board)
            |> List.fold (fun queue item -> queue |> PriorityQueue.insert item) queue

        // Console.ReadKey() |> ignore

        find queue
    find queue

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> printfn "%A"
    // let board = testInput |> parse
    // nextPositions board |> printfn "%A"
    // shortestPath board |> printfn "%A"
    testInput |> parse |> shortestPath |> printfn "%A"

    // input |> parse |> shortestPath |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
