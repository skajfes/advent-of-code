open System.Collections.Immutable
open System.IO

type Sandpit =
    { Map: ImmutableHashSet<int * int>
      Max: int }

let makeWalls (line: string) =
    line.Split("->")
    |> Array.map (fun c -> c.Split(','))
    |> Array.map (fun [| a; b |] -> int a, int b)
    |> Array.pairwise
    |> Array.map (fun ((a, b), (c, d)) ->
        match a, b, c, d with
        | _ when a = c && b > d -> [| for i in d..b -> a, i |]
        | _ when a = c -> [| for i in b..d -> a, i |]
        | _ when b = d && c > a -> [| for i in a..c -> i, b |]
        | _ when b = d -> [| for i in c..a -> i, b |]
        | _ -> failwith "unable to do wall")
    |> Array.concat

let parse (input: string[]) =
    input
    |> Array.map makeWalls
    |> Array.concat
    |> Array.fold (fun (hash: ImmutableHashSet<int * int>) coord -> hash.Add(coord)) ImmutableHashSet.Empty
    |> fun map ->
        { Map = map
          Max = map |> Seq.map snd |> Seq.max }


let pourOneGrain hasBottom (pit: Sandpit) =
    let start = (500, 0)

    let rec pour (x, y) =
        if hasBottom && y = pit.Max - 1 then
            Some(x, y)
        else if y > pit.Max then
            None
        else if pit.Map.Contains(x, y + 1) |> not then
            pour (x, y + 1)
        elif pit.Map.Contains(x - 1, y + 1) |> not then
            pour (x - 1, y + 1)
        elif pit.Map.Contains(x + 1, y + 1) |> not then
            pour (x + 1, y + 1)
        else
            Some(x, y)

    pour start

let rec pourSand hasBottom count (pit: Sandpit) =
    match pourOneGrain hasBottom pit with
    | None -> count
    | Some (500, 0) -> count + 1
    | Some coord -> { pit with Map = pit.Map.Add(coord) } |> pourSand hasBottom (count + 1)

let part1 pit = pourSand false 0 pit

let part2 pit =
    { pit with Max = pit.Max + 2 } |> pourSand true 0

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
