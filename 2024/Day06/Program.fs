open System.Collections.Generic
open System.IO

type Position =
    | Wall of int*int
    | Guard of int*int
    | Nothing

type Direction = Up | Down | Left | Right

let parse (input: string[]) =
    let n = Array.length input

    input
    |> Array.mapi (fun r row ->
        row
        |> Seq.mapi (fun c v ->
            match v with
            | '#' -> Wall (r, c)
            | '^' -> Guard (r, c)
            | '.' -> Nothing
            | _ -> failwith "nope"
        )
    )
    |> Seq.concat
    |> Seq.fold (fun (walls, guard) e ->
        match e with
        | Wall (r, c) -> (r, c)::walls, guard
        | Guard (r, c) -> walls, (r, c)
        | Nothing -> walls, guard
    ) ([], (0,0))
    // return size of grid, position of walls and position of guard
    |> fun (walls, guard) -> n, walls, guard

let guard_step (walls: HashSet<int*int>) (gr, gc) direction =
    let g' =
        match direction with
        | Up -> gr - 1, gc
        | Down -> gr + 1, gc
        | Left -> gr, gc - 1
        | Right -> gr, gc + 1

    if walls.Contains(g') then None else Some g'

let turn_right =
    function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let find_guard_path n walls g =
    let path = HashSet<(int*int)*Direction>()
    let rec find g dir =
        match guard_step walls g dir with
        | Some (gr, gc) when gr < 0 || gc < 0 || gr >= n || gc >= n -> Some path
        | Some g' when path.Contains(g', dir) -> None
        | Some g' ->
            path.Add(g', dir) |> ignore
            find g' dir
        | None -> find g (turn_right dir)
    match find g Up with
    | Some path ->
        path
        |> Seq.toList
        |> List.map fst
        |> List.distinct
    | _ -> failwith "no path"

// don't track path walked to detect infinite route, only count,
// assume if path long enough it's infinite
let find_inf_path n walls g =
    let rec find g dir len =
        match guard_step walls g dir with
        | Some (gr, gc) when gr < 0 || gc < 0 || gr >= n || gc >= n -> 0
        | Some _ when len > 5700 -> 1
        | Some g' -> find g' dir (len + 1)
        | None -> find g (turn_right dir) (len + 1)

    find g Up 0

let part1 (n, walls: (int*int)list, guard:int*int) =
    find_guard_path n (HashSet(walls)) guard
    |> List.length

let part2 (n, walls: (int*int)list, guard:int*int) =
    // Try adding wall on every step of path
    find_guard_path n (HashSet(walls)) guard
    |> List.sumBy (fun w -> find_inf_path n (HashSet(w::walls)) guard)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
