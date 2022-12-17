open System.Collections.Generic
open System.IO
open FSharpx.Collections

let parse (input: string[]) =
    input
    |> Seq.mapi (fun r -> Seq.mapi (fun c v -> (r, c), v))
    |> Seq.concat
    |> Map.ofSeq

let maxOneUp current_elevation next_elevation =
    match current_elevation, next_elevation with
    | _, 'E' when current_elevation = 'z' || current_elevation = 'y' -> true
    | _, 'E' -> false
    | 'S', _ when next_elevation = 'a' || next_elevation = 'b' -> true
    | 'S', _ -> false
    | _ when int next_elevation <= int current_elevation + 1 -> true
    | _ -> false

let maxOneDown current_elevation next_elevation =
    match current_elevation, next_elevation with
    | 'E', _ when next_elevation = 'z' || next_elevation = 'y' -> true
    | 'E', _ -> false
    | _, 'S' when current_elevation = 'a' || current_elevation = 'b' -> true
    | _, 'S' -> false
    | _ when int current_elevation - 1 <= int next_elevation -> true
    | _ -> false

let nextPositions compare map (r, c) current_elevation =
    [ (r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1) ]
    |> List.map (fun p -> p, Map.tryFind p map)
    |> List.filter (fun (_, elevation) -> elevation <> None)
    |> List.map (fun (p, Some elevation) -> p, elevation)
    |> List.filter (fun (_, next_elevation) -> compare current_elevation next_elevation)

let shortestPath start stop compare map =
    let start_pos = Map.findKey (fun r v -> v = start) map

    let visited = HashSet<int * int>()
    let queue = PriorityQueue.empty false |> PriorityQueue.insert (0, start_pos, start)

    let rec find queue =
        let (steps, position, el), queue = PriorityQueue.pop queue

        if visited.Contains(position) then
            find queue
        elif el = stop then
            steps
        else

            visited.Add(position) |> ignore

            let queue =
                nextPositions compare map position el
                |> List.filter (fun (pos, v) -> visited.Contains(pos) |> not)
                |> List.fold (fun queue (pos, v) -> queue |> PriorityQueue.insert (steps + 1, pos, v)) queue

            find queue

    find queue

let part1 map = shortestPath 'S' 'E' maxOneUp map

let part2 map =
    map
    |> Map.map (fun pos v -> if v = 'S' then 'a' else v)
    |> shortestPath 'E' 'a' maxOneDown

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
