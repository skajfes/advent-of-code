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

let exclusion_row row ((sx, sy), (bx, by)) =
    let distance = abs (sx - bx) + abs (sy - by)
    let remaining = distance - abs (sy - row)

    if remaining < 0 then
        None
    else
        Some(sx - remaining, sx + remaining)


let rec removeBeacons beacons ranges =
    match beacons, ranges with
    | [], _ -> ranges
    | _, [] -> []
    | b :: bs, (xmin, xmax) :: _ when b > xmax -> removeBeacons bs ranges
    | b :: bs, (xmin, xmax) :: rest when b < xmin -> (xmin, xmax) :: removeBeacons (b :: bs) rest
    | b :: bs, (xmin, xmax) :: rest when b = xmin && xmin = xmax -> removeBeacons bs rest
    | b :: bs, (xmin, xmax) :: rest when b = xmin -> removeBeacons bs ((xmin + 1, xmax) :: rest)
    | b :: bs, (xmin, xmax) :: rest when b = xmax -> removeBeacons bs ((xmin, xmax - 1) :: rest)
    | b :: bs, (xmin, xmax) :: rest -> removeBeacons bs ((xmin, b - 1) :: (b + 1, xmax) :: rest)

let part1 row sensors =
    let beacons =
        sensors
        |> Array.map snd
        |> Array.filter (snd >> (=) row)
        |> Array.map fst
        |> Array.sort
        |> Array.toList

    sensors
    |> Array.map (exclusion_row row)
    |> Array.filter ((<>) None)
    |> Array.map (function
        | Some x -> x
        | _ -> failwith "oops")
    |> Array.sortBy fst
    |> Array.fold
        (fun ranges range ->
            match ranges with
            | [] -> [ range ]
            | (rmin, rmax) :: ranges when rmax < fst range -> range :: (rmin, rmax) :: ranges
            | (rmin, rmax) :: ranges when rmin <= fst range && rmax >= snd range -> (rmin, rmax) :: ranges
            | (rmin, rmax) :: ranges -> (rmin, snd range) :: ranges)
        []
    |> removeBeacons beacons
    |> List.map (fun (min, max) -> max - min + 1)
    |> List.sum


let toLines ((sx, sy), (bx, by)) =
    let distance = abs (sx - bx) + abs (sy - by) + 1 // manhattan distance + 1 to get the points just outside sensor reach

    let point_up = (sx, sy + distance)
    let point_down = (sx, sy - distance)
    let point_left = (sx - distance, sy)
    let point_right = (sx + distance, sy)

    let line_1 = (1, sy - sx + distance)
    let line_2 = (1, sy - sx - distance)
    let line_3 = (-1, sy + sx - distance)
    let line_4 = (-1, sy + sx + distance)

    [| (point_left, point_up), line_1
       (point_down, point_right), line_2
       (point_left, point_down), line_3
       (point_up, point_right), line_4 |]

let areParallel (_, (a1, _)) (_, (a2, _)) = a1 = a2

let xOutOfRange (((ax1, _), (ax2, _)), _) (((bx1, _), (bx2, _)), _) =
    (ax1 <= bx1 && bx1 <= ax2) || (ax1 <= bx2 && bx2 <= ax2) |> not

let intersect (((ax1, ay1), (ax2, ay2)), (aa, ab)) (((bx1, by1), (bx2, by2)), (ba, bb)) =
    let xd = decimal (bb - ab) / decimal (aa - ba)
    let x = int xd

    if xd <> round xd then
        None
    elif x < ax1 || x > ax2 || x < bx1 || x > bx2 then
        None
    else
        let x = int x
        let y = aa * x + ab

        let [ ay1; ay2 ] = [ ay1; ay2 ] |> List.sort
        let [ by1; by2 ] = [ by1; by2 ] |> List.sort

        if y < ay1 || y > ay2 || y < by1 || y > by2 then
            None
        else
            Some(x, y)

let intersectWithSensor size sa sb =
    Array.allPairs sa sb
    |> Array.map (fun (lineA, lineB) ->
        match lineA, lineB with
        | _ when areParallel lineA lineB -> None
        | _ when xOutOfRange lineA lineB -> None
        | _ -> intersect lineA lineB)
    |> Array.filter ((<>) None)
    |> Array.filter (fun (Some (x, y)) -> x >= 0 && x <= size && y >= 0 && y <= size)

let intersectWith size sensors sensor =
    sensors
    |> Array.map (intersectWithSensor size sensor)
    |> Array.concat
    |> Array.map (function
        | Some x -> x
        | None -> failwith "oops")


let findIntersections size sensors =
    sensors |> Array.map (intersectWith size sensors) |> Array.concat

let outOfRange sensors (x, y) =
    sensors
    |> Array.map (fun ((x1, y1), (x2, y2)) -> (x1, y1), abs (x2 - x1) + abs (y2 - y1))
    |> Array.forall (fun ((sx, sy), r) -> abs (sx - x) + abs (sy - y) > r)


let part2 size sensors =
    sensors
    |> Array.map toLines
    |> findIntersections size
    |> Array.filter (outOfRange sensors)
    |> Array.distinct
    |> Array.head
    |> fun (x, y) -> 4_000_000L * (int64 x) + (int64 y)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> part1 10 |> printfn "%A"
    // testInput |> parse |> part2' 20 |> printfn "%A"

    input |> parse |> part1 2_000_000 |> printfn "Part 1: %A"
    input |> parse |> part2 4_000_000 |> printfn "Part 2: %d"

    0 // return an integer exit code
