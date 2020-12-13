open System
let testInput =
 """F10
N3
F7
R90
F11""".Split("\r\n")

type Command =
    | Forward of int
    | Left of int
    | Right of int
    | North of int
    | East of int
    | South of int
    | West of int
    
let (|Prefix|_|) (prefix: string) (input: string) =
    if input.StartsWith(prefix)
    then input.Substring(prefix.Length) |> int |> Some
    else None
    
let parse input =
    input
    |> Array.map (function
        | Prefix "F" x -> Forward x
        | Prefix "L" x -> Left x
        | Prefix "R" x -> Right x
        | Prefix "N" x -> North x
        | Prefix "S" x -> South x
        | Prefix "W" x -> West x
        | Prefix "E" x -> East x
        | _ -> failwith "unknown command")
    
let follow instructions =
    instructions
    |> Array.fold (fun (x, y, dir) ->
        function
        | Forward d ->
            match dir with
            | 0 -> (x, y + d, dir)
            | 90 -> (x + d, y, dir)
            | 180 -> (x, y - d, dir)
            | 270 -> (x - d, y, dir)
            | d -> failwithf "unknown dir %d" d
        | Left d -> (x, y, (dir - d + 360) % 360)
        | Right d -> (x, y, (dir + d + 360) % 360)
        | North d -> (x, y + d, dir)
        | South d -> (x, y - d, dir)
        | East d -> (x + d, y, dir)
        | West d -> (x - d, y, dir)
        ) (0, 0, 90)
    
let rotate (x, y) angle_diff =
    let distance = Math.Sqrt(float x**2.0 + float y**2.0)
    let angle = Math.Acos(float x / distance) * 180.0 / Math.PI * (if y < 0 then -1.0 else 1.0)
    let new_angle = angle - float angle_diff
    let new_x = Math.Cos(new_angle * Math.PI / 180.0) * distance
    let new_y = Math.Sin(new_angle * Math.PI / 180.0) * distance
    (int <| Math.Round(new_x), int <| Math.Round(new_y))
    
let followWaypoint instructions =
    instructions
    |> Array.fold (fun (x, y, (wp_x, wp_y)) ->
        function
        | Forward d -> (x + d * wp_x, y + d * wp_y, (wp_x, wp_y))
        | Left d -> (x, y, rotate (wp_x, wp_y) -d)
        | Right d -> (x, y, rotate (wp_x, wp_y) d)
        | North d -> (x, y, (wp_x, wp_y + d))
        | South d -> (x, y, (wp_x, wp_y - d))
        | East d -> (x, y, (wp_x + d, wp_y))
        | West d -> (x, y, (wp_x - d, wp_y))
        ) (0, 0, (10,1))
let distance (x:int, y:int, _) =
    Math.Abs(x) + Math.Abs(y)

[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllLines("input.txt")
    input |> parse |> follow |> distance |> printfn "Part 1: %d"
    input |> parse |> followWaypoint |> distance |> printfn "Part 2: %d"
    0 // return an integer exit code
    
    
