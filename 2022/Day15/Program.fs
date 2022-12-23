open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some <| List.tail [ for g in m.Groups -> g.Value ]
    else
        None

let parseLine line =
    match line with
    | Regex "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" [ sx; sy; bx; by ] ->
        (int sx, int sy), (int bx, int by)
    | _ -> failwith "unparsable"

let parse (input: string[]) = input |> Array.map parseLine

let distance_points = Dictionary<int, (int*int)[]>()
let pointsAtDistance distance =
    if distance_points.ContainsKey(distance) then distance_points[distance] else
    let points =
        [ for dx in 0..distance -> dx, distance - dx ]
        |> List.append [ for dx in 0..distance -> -dx, -1 * (distance - dx) ]
        |> List.append [ for dx in 0..distance -> dx, -1 * (distance - dx) ]
        |> List.append [ for dx in 0..distance -> -dx, (distance - dx) ]
        |> List.distinct
        |> List.toArray
    distance_points[distance] <- points
    points


let exclusion_zones = Dictionary<int, (int * int)[]>()

let exclusion_zone_deltas distance =
    if exclusion_zones.ContainsKey(distance) then
        exclusion_zones[distance]
    else
        let deltas = [| 0..distance |] |> Array.collect pointsAtDistance |> Array.distinct

        exclusion_zones[distance] <- deltas
        deltas

let exclusion_zone (sensor, beacon) =
    let distance = abs (fst sensor - fst beacon) + abs (snd sensor - snd beacon)

    exclusion_zone_deltas distance
    |> Array.map (fun (dx, dy) -> (fst sensor + dx, snd sensor + dy))

let exclusion_row row ((sx, sy), (bx, by)) =
    let distance = abs (sx - bx) + abs (sy - by)
    let remaining = distance - abs (sy - row)

    [| - remaining .. remaining |]
    |> Array.map (fun x -> sx + x, row)
    |> Array.filter ((<>)(bx, by))


let part1 row sensors =
    sensors
    |> Array.collect (exclusion_row row)
    |> Array.filter (fun (x, y) -> y = row)
    |> Array.distinct
    |> Array.length

let part2 limit sensors =
    let excluded =
        sensors
        |> Array.collect exclusion_zone

    [|0..limit|]
    |> Array.map (fun row -> excluded |> Array.filter (fun (x, y) -> y = row && x >= 0 && x <= limit))
    |> Array.mapi (fun r row ->
        [|0..limit|]
        |> Array.filter (fun x -> row |> Array.contains (x, r) |> not)
        |> Array.map (fun x -> x, r)
    )
    |> Array.concat
    |> Array.head
    |> fun (x, y) -> x * 4000000 + y

let toDistance ((sx, sy), (bx, by)) =
    (sx, sy), abs(sx-bx) + abs(sy-by)

let pointsOutOfRange ((sx, sy), range) =
    [| for dx in 0..range -> sx + dx, sy + range - dx |]
    |> Array.append [| for dx in 0..range -> sx-dx, sy - (range - dx) |]
    |> Array.append [| for dx in 0..range -> sx+dx, sy - (range - dx) |]
    |> Array.append [| for dx in 0..range -> sx-dx, sy + (range - dx) |]
    |> Array.distinct

let inRange (point: int * int) (sensor: int * int, range: int) =
    snd (toDistance (sensor, point)) <= range

let part2' size sensors =
    let sensors' =
        sensors
        |> Array.map toDistance
    sensors'
    |> Array.map pointsOutOfRange
    |> Array.concat
    |> Array.filter (fun (x, y) -> x >= 0 && x <= size && y >= 0 && y <= size)
    |> Array.filter (fun point -> sensors' |> Array.forall (inRange point >> not))
    |> Array.distinct
    |> Array.head
    |> fun (x, y) -> (int64 x) * (int64 size) + (int64 y)

// 2889605L*4_000_000L+3398893L

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part2' 20 |> printfn "%A"

    // input |> parse |> part1 2000000 |> printfn "Part 1: %A"
    input |> parse |> part2' 4_000_000 |> printfn "Part 2: %A"

    0 // return an integer exit code
