open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

type Cube = (int*int)*(int*int)*(int*int)

let parse (input: string[]): (bool * Cube)[] =
    input
    |> Array.map (function
        | Regex "^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" [state; xmin; xmax; ymin; ymax; zmin; zmax] ->
            (state = "on", ((int xmin, int xmax), (int ymin, int ymax), (int zmin, int zmax)))
        | _ -> failwith "invalid input"
        )

let totalVolume (cube: Cube, intersections: Cube list) =
    let volume ((x, y, z): Cube) =
        let length (x1, x2) =
            (abs (x2 - x1) |> int64) + 1L
        (length x) * (length y) * (length z)

    volume cube - List.sumBy volume intersections

let getIntersection ((xs1, ys1, zs1): Cube) ((xs2, ys2, zs2): Cube): Cube option =
    let find (amin, amax) (bmin, bmax) =
        if amin <= bmin && amax >= bmin && amax <= bmax then Some (bmin, amax) else
        if amin >= bmin && amin <= bmax && amax >= bmax then Some (amin, bmax) else
        if amin >= bmin && amin <= bmax && amax <= bmax then Some (amin, amax) else
        if amin <= bmin && amax >= bmax then Some (bmin, bmax)
        else None
    match find xs1 xs2, find ys1 ys2, find zs1 zs2 with
    | Some x, Some y, Some z -> Some (x, y, z)
    | _ -> None

let getIntersections cube cube_list =
    ([], cube_list)
    ||> List.fold (fun res item ->
        match getIntersection cube item with
        | Some x -> x::res
        | None -> res)

let intersect (cube: Cube) (cubes: (Cube * Cube list) list) =
    ([], cubes)
    ||> List.fold (fun intersections (c1, c1_intersections) ->
            match getIntersection cube c1 with
            | None -> intersections
            | Some x ->
                let prevIntersections =
                    getIntersections x c1_intersections
                (x, prevIntersections)::intersections
        )

let initialization (instructions: (bool * Cube)[]) =
    let inrange (xmin, xmax) =
        (xmin >= -50 && xmin <= 50) || (xmax >= -50 && xmax <= 50)
    instructions
    |> Array.filter (fun (_, (x, y, z)) -> inrange x && inrange y && inrange z)

let reboot (instructions: (bool * Cube)[]) =
    ([], instructions)
    ||> Array.fold (fun (processed: (Cube * Cube list) list) (state, cube) ->
        match state with
        | true ->
            let intersections = processed |> intersect cube
            let prevInters = intersections |> List.collect snd
            let intersections = intersections |> List.map fst

            // add cube with intersections
            // and add new cubes for each intersection with previous cubes
            ((cube, intersections)::processed, prevInters)
            ||> List.fold (fun processed c -> (c, [])::processed)
        | false ->
            ([], processed)
            ||> List.fold (fun processed (c1, c1_intersections) ->
                match getIntersection cube c1 with
                | Some x ->
                    // add intersection to existing cube intersections
                    let processed = ((c1, x::c1_intersections)::processed)
                    // add any intersections with previous intersections as new cubes
                    (processed, getIntersections x c1_intersections)
                    ||> List.fold (fun processed c -> (c, [])::processed)
                | None -> (c1, c1_intersections)::processed
            )
        )
    |> List.sumBy totalVolume

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    let testInput2 = File.ReadAllLines("sample2.txt")
    let testInput3 = File.ReadAllLines("sample3.txt")

    input |> parse |> initialization |> reboot |> printfn "Part 1: %d"
    input |> parse |> reboot |> printfn "Part 2: %d"

    0 // return an integer exit code
