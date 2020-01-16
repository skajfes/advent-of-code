// #load "IntCode.fs"

open System

let getmap robot = 
    let robot, output = IntCode.run [0L] robot 

    (output 
    |> List.mapFold (fun (row, col) e -> ((row, col), (char>>string)e), if e = 10L then (row+1, 0) else (row, col+1)) (0, 0)
    |> fst
    |> List.filter (snd>>(<>)"\n")
    |> Map.ofList, robot)

let iscross map ((row, col), _) =
    let neighbours e _ =
        [(-1,0);(1,0);(0,-1);(0,1)]
        |> List.map (fun (r, c) -> (r + row, c + col))
        |> List.contains e

    Map.filter (neighbours) map
    |> Map.forall (fun _ k -> k = "#")

let findIntersections map =
    Map.toList map
    |> List.filter (snd>>(=)"#")
    |> List.filter (iscross map)

let drawMap map =
    let l = Map.toList map
    let minX = List.map (fst >> fst) l |> List.min
    let minY = List.map (fst >> snd) l |> List.min
    let maxX = List.map (fst >> fst) l |> List.max
    let maxY = List.map (fst >> snd) l |> List.max

    //Console.SetCursorPosition(0,0)
    [for x in minX..maxX ->
        [for y in minY..maxY -> 
            match map.TryFind (x, y) with
            | Some d -> d
            | None -> " "
        ] |> String.concat "" 
    ] |> String.concat "\n" |> printfn "%s"
    //Console.ReadKey() |> ignore

type Direction =
    | North
    | South
    | West
    | East

type Robot = {
    Coord: int*int
    Direction: Direction
}

let findRobot map =
    let r = Map.filter (fun c (e:string) -> "^v<>".Contains(e)) map |> Seq.head
    let dir =
        match r.Value with
        | "^" -> North
        | "<" -> West
        | "v" -> South
        | ">" -> East
        | _ -> failwith "unknown direction"
    { Coord = r.Key; Direction = dir }

let rec findWay (r:Robot) map =
    let move dir =
        let dr, dc = 
            match dir with
            | North -> (-1,0)
            | South -> (1,0)
            | East -> (0, 1)
            | West -> (0, -1)

        let rec doMove (r, c) =
            match Map.tryFind (r + dr, c + dc) map with
            | Some "#" ->
                let (l, coord) = doMove (r + dr, c + dc)
                (1 + l, coord)
            | _ -> (0, (r, c))

        let l, c = doMove (r.Coord)
        (l, { r with Coord = c; Direction = dir })

    let moveLeft =
        match r.Direction with
        | North -> move West
        | West -> move South
        | South -> move East
        | East -> move North
    
    let moveRight =
        match r.Direction with
        | North -> move East
        | West -> move North
        | South -> move West
        | East -> move South       

    match (moveLeft, moveRight) with
    | ((0, _), (0, _)) -> []
    | ((l, robot), (0, _)) -> "L"::string l::findWay robot map
    | ((0, _), (r, robot)) -> "R"::string r::findWay robot map
    | _ -> failwith "can go left and right"
    

   
let compress (way:string list) =
    let directions =
        List.pairwise way 
        |> List.filter (fun (e, _) -> Char.IsLetter(e.[0]))
        |> List.map (fun (a, b) -> a + b)

    [for lenA in 1..12 ->
        let l = directions |> String.concat ""
        let a = List.take lenA directions |> String.concat ""    
        let withA = l.Replace(a, "A")
        [for lenB in 1..12 -> 
            let b = List.skip lenA directions |> List.take lenB |> String.concat ""
            let withB = withA.Replace(b, "B")
            [for lenC in 1..12 ->
                let startC = Seq.takeWhile (fun c -> c = 'A' || c = 'B') withB |> Seq.sumBy (function | 'A' -> lenA | 'B' -> lenB) 
                let c = List.skip (startC) directions |> List.take lenC |> String.concat ""
                let withC = withB.Replace(c, "C")
                (withC, (a, b, c))
            ]
        ]
    ]
    |> List.concat
    |> List.concat
    |> List.minBy (fst >> String.length)

let runRobot (main, (a, b, c)) (robot:IntCode.Program) =
    // force to move
    let robot = { robot with Memory = Map.add 0 2L robot.Memory}
    let toAscii (l:seq<char>) = 
        l 
        |> Seq.map (string) 
        |> String.concat "," 
        |> fun s -> s+"\n"
        |> fun i -> System.Text.RegularExpressions.Regex.Replace(i, @"(\d),(\d)", @"$1$2")

    let input = [toAscii main; toAscii a; toAscii b; toAscii c; "n\n"] |> String.concat "" |> Seq.map (int64) |> Seq.toList

    IntCode.run input robot

[<EntryPoint>]
let main argv =
    let robot = IO.File.ReadAllText("input.txt") |> IntCode.compile

    let map, r = getmap robot
    // let intersections = findIntersections map
    // List.sumBy (fun ((r, c), _) -> r*c) intersections

    drawMap map

    let r = findRobot map
    let way = findWay r map
    let path = compress way
    let r, o = runRobot path robot

    o |> List.rev |> List.head |> int


