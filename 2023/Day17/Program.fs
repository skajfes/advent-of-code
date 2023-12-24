open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun row -> row |> Seq.map (string>>int) |> Seq.toArray)

type Direction = Up | Down | Left | Right
type Step = (int * int) * int * Direction list

let opposite =
    function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let step (r, c) dir =
    match dir with
    | Up -> (r-1, c)
    | Down -> (r+1, c)
    | Left -> (r, c-1)
    | Right -> (r, c+1)

let turn_right dir =
    match dir with
    | Up -> Right
    | Down -> Left
    | Left -> Up
    | Right -> Down

let turn_left dir =
    match dir with
    | Up -> Left
    | Down -> Right
    | Left -> Down
    | Right -> Up

let coords (r, c) dir steps =
    match dir with
    | Up -> [for i in 1..steps -> (r - i, c)]
    | Down -> [for i in 1..steps -> (r + i, c)]
    | Left -> [for i in 1..steps -> (r, c - i)]
    | Right -> [for i in 1..steps -> (r, c + i)]

let neighbours1 (map: int array array) maxr maxc (r, c) dirs =
    [ (r - 1, c), Up
      (r + 1, c), Down
      (r, c - 1), Left
      (r, c + 1), Right ]
    // filter out of bounds
    |> List.filter (fun ((r, c), _) -> r >= 0 && c >= 0 && r <= maxr && c <= maxc)
    // filter out more than 3 same direction
    |> fun list ->
        match dirs with
        | x::y::z::_ when x = y && y = z -> list |> List.filter (snd >> (<>) x)
        | _ -> list
    |> List.map (fun (coord, dir) -> coord, [dir], map[fst coord][snd coord]) // add cost

let neighbours2 (map: int array array) maxr maxc (r, c) dirs =
    // if start - down or right 4 times
    // if any direction
    //  - same direction 1
    //      - except more than 10
    //  - turn left/right 4 spaces
    let last_dir, len =
        match dirs with
        | [] -> Up, 0
        | d::_ -> d, dirs |> List.takeWhile ((=)d) |> List.length

    if len = 0 then // start
        [ (r + 4, c), Down, [for i in 1..4 -> (r+i, c)]
          (r, c + 4), Right, [for i in 1..4 -> (r, c+i)] ]
        |> List.map (fun (coord, dir, steps) -> coord, List.replicate 4 dir, steps |> List.sumBy (fun (x, y) -> map[x][y])) // add cost
    else
        seq {
            if len < 10 then
                yield (last_dir, 1)
            yield (turn_right last_dir, 4)
            yield (turn_left last_dir, 4)
        }
        |> Seq.toList
        |> List.map (fun (dir, steps) -> coords (r, c) dir steps, List.replicate steps dir )
        |> List.filter (fun (coords, dirs) -> coords |> List.forall (fun (r, c) -> r >= 0 && r <= maxr && c >= 0 && c <= maxc))
        |> List.map (fun (coords, dirs) -> List.last coords, dirs, coords |> List.sumBy (fun (x, y) -> map[x][y]) )

let findPath neighbours_impl max_path_length map =
    let queue = PriorityQueue<Step, int>()
    let visited = HashSet<(int*int) * Direction list>()

    let maxr = Array.length map - 1
    let maxc = Array.length map[0] - 1
    let finish = (maxr, maxc)

    let neighbours (r, c) dirs =
        neighbours_impl map maxr maxc (r, c) dirs
        // filter out opposite direction
        |> fun list ->
            match List.tryHead dirs with
            | Some x -> list |> List.filter ((fun (a, b, c) -> List.head b) >> (<>) (opposite x))
            | None -> list

    let rec find() =
        let item, total_cost, dirs = queue.Dequeue()

        // printfn "%d %A %A" total_cost item dirs

        // at finish
        if item = finish then total_cost else

        // skip if already been to field
        if visited.Contains(item, dirs) then find() else

        visited.Add(item, dirs) |> ignore

        neighbours item dirs
        |> List.iter (fun (coord, new_dirs, item_cost) ->
            queue.Enqueue((coord, item_cost + total_cost, List.append new_dirs dirs |> List.truncate (max_path_length + 1) ), item_cost + total_cost))

        find()

    queue.Enqueue(((0,0), 0, []), 0)
    find()

let part1 map =
    findPath neighbours1 3 map

let part2 map =
    findPath neighbours2 10 map

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    let testInput = File.ReadAllLines("sample2.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
