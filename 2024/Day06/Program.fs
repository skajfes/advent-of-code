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
        )
    )
    |> Seq.concat
    |> Seq.fold (fun (walls, guard) e ->
        match e with
        | Wall (r, c) -> (r, c)::walls, guard
        | Guard (r, c) -> walls, (r, c)
        | Nothing -> walls, guard
    ) ([], (0,0))
    |> fun (walls, guard) -> n, walls, guard

let guard_step walls (gr, gc) direction =
    let g' =
        match direction with
        | Up -> gr - 1, gc
        | Down -> gr + 1, gc
        | Left -> gr, gc - 1
        | Right -> gr, gc + 1

    if List.contains g' walls then None else Some g'

type Res = Inf | OOB of HashSet<(int*int)*Direction>

let rec guard_walk n walls g dir (path: HashSet<(int*int)*Direction>) =
    match guard_step walls g dir with
    | Some (gr, gc) when gr < 0 || gc < 0 || gr >= n || gc >= n -> OOB path
    | Some g' when path.Contains(g', dir) -> Inf
    | Some g' ->
        path.Add(g', dir) |> ignore
        guard_walk n walls g' dir path
    | None ->
        match dir with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
        |> fun dir -> guard_walk n walls g dir path

let part1 (n, walls: (int*int)list, guard:int*int) =
    match guard_walk n walls guard Up (HashSet<(int*int)*Direction>()) with
    | OOB path ->
        path
        |> Seq.toList
        |> List.map fst
        |> List.distinct
        |> List.length

let part2 (n, walls: (int*int)list, guard:int*int) =
    let path =
        match guard_walk n walls guard Up (HashSet<(int*int)*Direction>()) with
        | OOB path ->
            path
            |> Seq.toList
            |> List.map fst
            |> List.distinct

    path
    |> List.map (fun (r, c) -> [r-1, c; r+1,c; r, c-1; r,c+1])
    |> List.concat
    |> List.distinct
    |> List.sort
    // [for r in 0..n-1 do for c in 0..n-1 -> r,c] // all coords
    |> List.mapi (fun i w ->
        if i % 100 = 0 then printfn "%d %A" i w
        guard_walk n (w::walls) guard Up (HashSet<(int*int)*Direction>()))
    |> List.filter (function Inf -> true | _ -> false)
    |> List.length


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    // input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
