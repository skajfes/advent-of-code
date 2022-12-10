open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type File = string * int64
type DirPoint = string
type Dir = string * DirPoint list * File list

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

let parse (input: string[]) =
    (([], []), input)
    ||> Array.fold (fun (dirs, path) line ->
        match line with
        | Regex "\$ ls" _ -> dirs, path
        | Regex "\$ cd \.\." _ -> dirs, match path with [] -> ["/"] | _ -> List.tail path
        | Regex "\$ cd (.+)" [dir] -> Dir ((dir :: path) |> List.rev |> String.concat "/", [], [])::dirs, dir :: path
        | Regex "dir (.+)" [dir] ->
            let (name, dirpoints, files)::dirs = dirs
            (name, (dir :: path |> List.rev |> String.concat "/")::dirpoints, files)::dirs, path
        | Regex "(\d+) (.+)$" [size; file] ->
            let (name, dirpoints, files)::dirs = dirs
            (name, dirpoints, (file, int64 size)::files)::dirs, path
        | _ -> failwith "error"
    )
    |> fst

let dirSize dirs name =
    let mem = Dictionary<string, int64>()

    let rec size name =
        if mem.ContainsKey(name) then
            mem[name]
        else
            let _, d, files = dirs |> List.find (fun (dir, _, _) -> dir = name)
            let s = (files |> List.sumBy snd) + (d |> List.sumBy size)
            mem.Add(name, s)
            s

    size name

let part1 (dirs: Dir list) =
    dirs
    |> List.map (fun (name, _, _) -> dirSize dirs name)
    |> List.filter (fun s -> s <= 100_000)
    |> List.sum

let part2 (dirs: Dir list) =
    let sizes =
        dirs
        |> List.map (fun (name, _, _) -> dirSize dirs name)
    let totalSize = dirSize dirs "/"
    let freeSpace = 70_000_000L - totalSize
    let toFree = 30_000_000L - freeSpace

    sizes
    |> List.sort
    |> List.filter (fun x -> x >= toFree)
    |> List.head


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
