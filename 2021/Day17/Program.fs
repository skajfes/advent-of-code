open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

let parse (input: string) =
    match input with
    | Regex "x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" [x1; x2; y1; y2] -> (int x1, int y1), (int x2, int y2)
    | _ -> failwith "no match"


let between x x1 x2 =
    if x1 > x2 then x >= x2 && x <= x1
    else x >= x1 && x <= x2

let inArea ((x1, y1), (x2, y2)) (x,y) =
    between x x1 x2 && between y y1 y2

let overArea ((x1, y1), (x2, y2)) (x, y) =
    (x > x2 && x > x1) || (y < y1 && y < y2)


let changeSpeed vx vy =
    let vx =
        match vx with
        | 0 -> 0
        | vx when vx > 0 -> vx - 1
        | vx when vx < 0 -> vx + 1

    let vy = vy - 1
    vx, vy
let rec testSpeed (x, y) targetArea (vx, vy) =
    // printfn "pos: %d, %d, vel: %d %d" x y vx vy
    let x, y = x + vx, y + vy
    if inArea targetArea (x, y) then true
    else if overArea targetArea (x, y) then false
    else testSpeed (x, y) targetArea (changeSpeed vx vy)

let maxY (vx, vy) =
    [0..vy]
    |> List.sum

let part1 targetArea =
    [for x in [0..20] do for y in [0..1000] -> x, y]
    |> List.filter (testSpeed (0, 0) targetArea)
    |> List.map maxY
    |> List.max

let part2 targetArea =
    [for x in [0..500] do for y in [-1000..1000] -> x, y]
    |> List.filter (testSpeed (0, 0) targetArea)
    |> List.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = "target area: x=20..30, y=-10..-5"
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
