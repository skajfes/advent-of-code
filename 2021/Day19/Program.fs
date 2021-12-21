open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

type Beacon = int * int * int
type Scanner = {
    Id: string
    Beacons: Beacon list
    Offset: int*int*int
}

let parse (input: string[]): Scanner list =
    ([], input)
    ||> Array.fold (fun scanners row ->
        match row with
        | Regex "--- (.*) ---" [name] -> { Id = name; Beacons = []; Offset = 0,0,0 }::scanners
        | Regex "^(-?\d+),(-?\d+),(-?\d+)$" [x; y; z] ->
            let last = List.head scanners
            { last with Beacons = (int x, int y, int z) :: last.Beacons } :: (List.tail scanners)
        | _ -> scanners)
    |> List.rev

let translateBeacons (x,y,z) beacons =
    let x1, y1, z1 = List.head beacons
    let dx, dy, dz = x-x1, y-y1, z-z1

    beacons
    |> List.map (fun (x, y, z) -> x+dx, y+dy, z+dz), (dx,dy,dz)

let translateAllBeacons (x, y, z) beacons =
    beacons
    |> List.map (fun (x1, y1, z1) ->
        let dx, dy, dz = x-x1, y-y1, z-z1

        beacons
        |> List.map (fun (x, y, z) -> x+dx, y+dy, z+dz), (dx,dy,dz) )

let countOverlap beacons1 beacons2 =
    beacons1
    |> List.filter (fun b -> beacons2 |> List.contains b)
    |> List.length

let rotateX (x: int, y: int, z: int) = x, -z, y
let rotateY (x: int, y: int, z: int) = z, y, -x
let rotateZ (x: int, y: int, z: int) = -y, x, z

let ff setup = [
    setup >> id
    setup >> rotateY
    setup >> rotateY >> rotateY
    setup >> rotateY >> rotateY >> rotateY
]
let rotations =
    [
        id
        rotateX
        rotateX >> rotateX
        rotateX >> rotateX >> rotateX
        rotateZ
        rotateZ >> rotateZ >> rotateZ
    ]
    |> List.collect ff

let rotate beacons =
    rotations
    |> List.map (fun f -> List.map f beacons)

let findWithRotation allBeacons testBeacons =
    testBeacons
    |> rotate
    |> Seq.collect (fun beacons ->
        allBeacons
        |> List.collect (fun b -> translateAllBeacons b beacons))
    |> Seq.filter (fun l -> fst l |> countOverlap allBeacons |> fun c -> c >= 12)
    |> Seq.tryHead

let distanceTo beacons (x, y, z) =
    beacons
    |> List.map (fun (x1, y1, z1) -> (pown (x1-x) 2 + pown (y1-y) 2 + pown (z1-z) 2))
    |> List.sort
    |> List.tail
    |> Set.ofList

let relativeDistances scanner =
    scanner.Beacons
    |> List.map (distanceTo scanner.Beacons)
    |> fun x -> x, scanner

let rec testDistance (distances: Set<int> list) (scanners: (Set<int> list * Scanner) list) =
    scanners
    |> List.filter (fun (d, s) ->
        // printfn "comparing with %A" s.Id
        d
        |> List.filter (fun dist ->
            distances
            |> List.map (Set.intersect dist >> Set.count)
            |> List.filter (fun c -> c >= 11)
            |> List.isEmpty |> not )
        |> List.isEmpty |> not
       )

let mapTo scanner0 scanner =
    printfn "fitting %A %A" scanner0.Id scanner.Id
    match findWithRotation scanner0.Beacons scanner.Beacons with
    | Some (beacons, offset) -> { scanner with Beacons = beacons; Offset = offset }
    | None -> failwithf "should not happen %A with %A" scanner0.Id scanner.Id

let rec findDistanceMatches (found: (Set<int> list * Scanner) list) (rest: (Set<int> list * Scanner) list) =
    ([], found)
    ||> List.fold (fun found (distances, scanner) ->
        // printfn "testing %A" scanner.Id
        match testDistance distances rest with
        | [] -> found
        | list ->
            // printfn "found %A" (list |> List.map (fun (d, s) -> s.Id))
            list
            |> List.map (fun (d, s) -> d, mapTo scanner s)
            |> List.append found)
    |> function
        | [] -> []
        | newFound ->
            let rest = rest |> List.filter (fun (_, s) -> newFound |> List.filter (fun (_, ss) -> ss.Id = s.Id) |> List.isEmpty)
            findDistanceMatches newFound rest
            |> List.append newFound

let findMatches (distances: (Set<int> list * Scanner) list) =
    let scanner0::scanners = distances

    findDistanceMatches [scanner0] scanners
    |> fun l -> scanner0::l
    |> List.map snd

let part1 scanners =
    scanners
    |> List.fold (fun beacons scanner ->
        beacons
        |> List.append scanner.Beacons) []
    |> List.distinct
    |> List.length

let part2 scanners =
    let scannerCoords =
        scanners
        |> List.map (fun s -> s.Offset)
    scannerCoords
    |> List.map (fun (x, y, z) ->
        scannerCoords
        |> List.map (fun (x1, y1, z1) -> (abs (x-x1) + abs(y-y1) + abs (z-z1))))
    |> List.concat
    |> List.max


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"

    let scanners =
        input
        |> parse
        |> List.map relativeDistances
        |> findMatches
    scanners |> part1 |> printfn "Part 1: %A"
    scanners |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
