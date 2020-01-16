open System
open System.IO

//#load "IntCode.fs"

type Direction =
    | North
    | South
    | East
    | West

let opposite dir =
    match dir with
    | North -> South
    | East -> West
    | South -> North
    | West -> East

type Result =
    | Wall
    | Ok
    | FoundO2

let move droid direction =
    let dir =
        match direction with
        | North -> 1L
        | South -> 2L
        | West -> 3L
        | East -> 4L

    let droid, output = IntCode.run dir droid

    let status =
        match List.head output with
        | 0L -> Wall
        | 1L -> Ok
        | 2L -> FoundO2
        | _ -> failwith "invalid status"

    droid, status

let rec navigate droid ignoreDir cnt : Direction list list =
    if cnt = 0 then [] else

    let moveDirection dir = 
        let droid, result = move droid dir
        match result with
        | Wall -> []
        | FoundO2 -> [dir]
        | Ok -> 
            let paths = navigate droid (opposite dir) (cnt - 1)
            let path = List.filter (List.isEmpty >> not) paths |> List.tryHead
            match path with
            | Some p -> dir :: p
            | _ -> []

    [North;South;East;West]
    |> List.filter ((<>)ignoreDir) 
    |> List.fold (fun result dir -> moveDirection dir :: result) []
    |> List.filter (List.isEmpty >> not)

let toCoord (x, y) dir = 
    match dir with
    | North -> (x, y-1)
    | South -> (x, y+1)
    | East -> (x+1, y)
    | West -> (x-1, y)

let drawMap map =
    let l = Map.toList map
    let minX = List.map (fst >> fst) l |> List.min
    let minY = List.map (fst >> snd) l |> List.min
    let maxX = List.map (fst >> fst) l |> List.max
    let maxY = List.map (fst >> snd) l |> List.max
    // List.map (fun ((x, y), d) -> ((x + minX, y + minY), d)) l

    Console.SetCursorPosition(0,0)
    [for x in minX..maxX ->
        [for y in minY..maxY -> 
            match map.TryFind (x, y) with
            | Some d -> 
                match d with
                | Wall -> "█"
                | Ok -> "."
                | FoundO2 -> "O"
            | None -> " "
        ] |> String.concat "" 
    ] |> String.concat "\n" |> printfn "%s"
    //Console.ReadKey() |> ignore

let rec createMap droid map coord : Map<int*int,Result> =
    let moveDirection map (dir, coord) = 
        let droid, result = move droid dir
        let map = Map.add coord result map
        match result with
        | Ok -> createMap droid map coord
        | _ -> map

    if Map.isEmpty map |> not 
    then drawMap map

    [North;South;East;West]
    |> List.map (fun d -> (d, toCoord coord d))
    |> List.filter (fun (_, c) -> Map.containsKey c map |> not)
    |> List.fold (moveDirection) map

let rec fillOxygen map depth =
    let os = Map.filter (fun c d -> d = FoundO2) map
    let full = Map.filter (fun c d -> d = Ok) map |> Map.count = 0
    if full then depth else

    //printfn "%d" depth
    //drawMap map
    //System.Console.ReadKey() |> ignore
    
    let map =
        Map.fold (fun map c _ -> 
            [North; South; East; West]
            |> List.map (toCoord c)
            |> List.fold (fun (map:Map<int*int,Result>) c -> 
                if map.[c] = Ok then
                    Map.add c FoundO2 map 
                else 
                    map
            ) map
        ) map os

    drawMap map
    fillOxygen map (depth + 1)


let testMap =
    [" ##   ";
    "#..## ";
    "#.#..#";
    "#.O.# ";
    " ###  "]
    |> List.mapi (fun x r -> 
        List.mapi (fun y e ->
            let t = 
                match e with
                | "#" -> Some Wall
                | "." -> Some Ok
                | "O" -> Some FoundO2
                | _ -> None
            ((x, y), t)
        ) (r.ToCharArray() |> Array.toList |> List.map string)
        |> List.filter (snd >> ((<>) None))
        |> List.map (fun (c, (Some t)) -> (c, t))
    )
    |> List.concat
    |> Map.ofList

[<EntryPoint>]
let main argv =
    let droid = File.ReadAllText ("input.txt") |> IntCode.compile

    //drawMap testMap
    // part 1
    // let r = navigate droid North 1000
    // List.length r.[0]

    Console.Clear()
    let map = createMap droid Map.empty (0, 0)
    fillOxygen map 0

    0
    
