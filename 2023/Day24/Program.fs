open System
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun line -> line.Split([|'@'; ',' |]) |> Array.map (_.Trim()) |> Array.map decimal)
    |> Array.map (fun [| px; py; pz; vx; vy; vz |] -> ((px, py, pz), (vx, vy, vz)))
    |> Array.toList

let point ((x, y, z), (vx, vy, vz)) t =
    (x + t*vx, y + t*vy, z + t*vz)

let find_params ((ax, ay, az), (avx, avy, avz)) ((bx, by, bz), (bvx, bvy, bvz)) =
    let B = bvx / bvy
    let d = B * avy - avx
    if abs d < 0.0001m then None else
    let t = (B * (by - ay) + ax - bx) / d
    let s = (ax - bx + t*avx) / bvx
    Some (t, s)

let (===) (ax, ay, az) (bx, by, bz) =
    let eq a b = (abs (a - b)) < 0.00001m
    // let eq = (=)
    eq ax bx && eq ay by && eq az bz

let intersect_lines (min_monitor, max_monitor) a b =
    let in_monitor (x, y, z) =
        min_monitor <= x && x <= max_monitor &&
        min_monitor <= y && y <= max_monitor &&
        min_monitor <= z && z <= max_monitor

    match find_params a b with
    | Some x -> Some x
    | None -> find_params b a
    |> function
        | Some (t, s) when t >= 0m && s >= 0m ->
            let a' = point a t
            let b' = point b s
            a' === b' && in_monitor a'
        | _ -> false

let rec pairs lines =
    match lines with
    | a::lines ->
        lines
        |> List.fold (fun res b -> (a, b)::res) []
        |> List.append (pairs lines)
    | _ -> []

let ignore_z v ((x, y, z), (vx, vy, vz)) = ((x, y, v), (vx, vy, 0m))

let part1 monitor_area hailstorm =
    hailstorm
    |> List.map (ignore_z (fst monitor_area))
    |> pairs
    |> List.filter (fun (a, b) -> intersect_lines monitor_area a b)
    |> List.length

let to_array (a, b, c, d, e) = [| a; b; c; d; e |]

let normalize (a: decimal array) t =
    let t = a[t]
    a |> Array.map (fun x -> x / t)

let subst t (a: decimal array) (a': decimal array) =
    let z = a'[t]/a[t]
    a' |> Array.mapi (fun i x -> x - z * a[i])

let rec gaussian_elimination_part1 t m =
    match m with
    | [] -> []
    | row::rest ->
        let row = normalize row t
        let rest' =
            rest
            |> List.map (subst t row)
            |> gaussian_elimination_part1 (t + 1)
        row :: rest'

let rec gaussian_elimination_part2 t m =
    match m with
    | [] -> []
    | row::rest ->
        let rest' =
            rest
            |> List.map (subst (t - 1) row)
            |> gaussian_elimination_part2 (t - 1)
        row :: rest'

let to_linear ((x, y, z), (dx, dy, dz)) =
    [| dy; -dx; -y; x; x*dy - y*dx |]

let to_linear2 ((x, y, z), (dx, dy, dz)) =
    [| dy; -dz; -y; z; z*dy - y*dz |]

let two_variable_linear to_linear hailstorm =
    let first::m =
        hailstorm
        |> List.take 5
        |> List.map to_linear

    let xy_array =
        m
        |> List.map (fun a -> a |> Array.zip first |> Array.map (fun (a, b) -> a - b))
        |> gaussian_elimination_part1 0
        |> List.rev
        |> gaussian_elimination_part2 4
        |> List.rev

    let x = xy_array[0][4]
    let y = xy_array[1][4]
    x, y

let part2 hailstorm =
    let x, y = two_variable_linear to_linear hailstorm
    let z, _ = two_variable_linear to_linear2 hailstorm

    x+y+z |> int64


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 (7m, 27m) |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%d"

    input |> parse |> part1 (200000000000000m, 400000000000000m) |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
