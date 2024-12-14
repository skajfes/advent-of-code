open System.IO
open FParsec

let p_coord = pint32 .>> pchar ',' .>>. pint32
let p_robot = pstring "p=" >>. p_coord .>> spaces .>> pstring "v=" .>>. p_coord

let parse_robot line =
    match run p_robot line with
    | Success (r, _ ,_) -> r
    | Failure (s, _, _) -> failwith s

let parse (input: string[]) =
    input
    |> Array.map parse_robot

let simulate_robot (max_x, max_y) time ((x, y), (vx, vy)) =
    let x' = (x + vx * time) % max_x
    let y' = (y + vy * time) % max_y
    let x' = (x' + max_x) % max_x
    let y' = (y' + max_y) % max_y
    x', y'

let split_quadrants (max_x, max_y) (x, y) =
    let xs = ((decimal max_x) / 2m |> floor |> int)
    let ys = ((decimal max_y) / 2m |> floor |> int)
    if x < xs && y < ys then 1
    elif x < xs && y > ys then 2
    elif x > xs && y < ys then 3
    elif x > xs && y > ys then 4
    else 0

let part1 size time robots =
    robots
    |> Array.map (simulate_robot size time)
    |> Array.countBy (split_quadrants size)
    |> Array.filter (fst >> (<)0)
    |> Array.map snd
    |> Array.fold (*) 1

let draw robots =
    [for r in 0..102 do
        seq [for c in 0..101 -> if Array.contains (c, r) robots then "*" else " " ]
        |> String.concat ""]
    |> String.concat "\n"

let part2 robots =
    seq [0..30000]
    |> Seq.map (fun i -> i, robots |> Array.map (simulate_robot (101, 103) i))
    |> Seq.filter (fun (i, robots) ->
        // looking for position with more than 30 robots in a row and column (the image is surrounded by square)
        let has_row = robots |> Array.countBy snd |> Array.exists (snd >> (<) 30)
        let has_column = robots |> Array.countBy fst |> Array.exists (snd >> (<) 30)

        has_row && has_column)
    |> Seq.map (fun (i, robots) ->
        // draw robots to screen
        draw robots |> printfn "%s"
        printfn "\n%A" i
        i
    )
    |> Seq.head

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 (11, 7) 100 |> printfn "%A"
    
    input |> parse |> part1 (101, 103) 100 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
