open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open FSharpx.Collections

type Item =
    | Chip of string
    | Generator of string

type Floor = {
    Chips: string list
    Generators: string list
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

let toItem item =
    match item with
    | Regex "a (.*) generator" [element] -> Generator element |> Some
    | Regex "a (.*)-compatible microchip" [element] -> Chip element |> Some
    | Regex "nothing relevant" [] -> None
    | _ -> failwithf "wat %s" item

let unpackItems (itemsLine: string) =
    itemsLine.Split([| ", "; " and "; "." |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun e -> e.Trim())
    |> Array.map toItem
    |> Array.filter (function | Some _ -> true | _ -> false)
    |> Array.map (function | Some x -> x)
    |> Array.sort

let toFloor items =
    let chips, generators =
        (([], []), items)
        ||> Array.fold (fun (chips, generators) ->
            function
                | Chip c -> c::chips, generators
                | Generator g -> chips, g::generators)

    {
        Chips = chips
        Generators = generators
    }

let parseLine (line: string) =
    match line with
    | Regex "The (.*) floor contains (.*)" [floor; items] ->
        let unpacked = unpackItems items
        match floor with
        | "first" -> 1, toFloor unpacked
        | "second" -> 2, toFloor unpacked
        | "third" -> 3, toFloor unpacked
        | "fourth" -> 4, toFloor unpacked
    | _ -> failwithf "unknown: %s" line

let parse (input: string[]) =
    input
    |> Array.map parseLine
    |> Map.ofArray

let anyChipWithoutGenerator items =
    let chips = items.Chips
    let generators = items.Generators

    chips
    |> List.exists (fun c ->
        let hasOwnGenerator = generators |> List.exists ((=)c)
        let hasOtherGenerators = generators |> List.exists ((<>)c)
        hasOtherGenerators && not hasOwnGenerator)

let isValid map =
    map
    |> Map.filter (fun _ items -> anyChipWithoutGenerator items)
    |> Map.isEmpty

let moveWith1item fromFloor toFloor map =
    let source = Map.find fromFloor map
    let target = Map.find toFloor map

    [
        yield! source.Chips
            |> List.map (fun item ->
                toFloor, map
                |> Map.add fromFloor { source with Chips = source.Chips |> List.filter ((<>) item) }
                |> Map.add toFloor { target with Chips = item :: target.Chips } )


        yield! source.Generators
            |> List.map (fun item ->
                toFloor, map
                |> Map.add fromFloor { source with Generators = source.Generators |> List.filter ((<>) item) }
                |> Map.add toFloor { target with Generators = item :: target.Generators } )
    ]

let moveWith2items floor toFloor map =
    map
    |> moveWith1item floor toFloor
    |> List.collect (fun (f, m) -> moveWith1item floor toFloor m)

let printMap (floor, map) =

    // let index = map
    //             |> Map.toList
    //             |> List.map snd
    //             |> List.collect id
    //             |> List.sortBy (function
    //                 | Chip c -> c, 2
    //                 | Generator g -> g, 1)
    let index =
        map
        |> Map.toList
        |> List.collect (snd >> (fun x -> x.Generators) )
        |> List.sort
        |> List.distinct
        |> List.collect (fun x -> [Generator x; Chip x])


    let toString = function
        | Chip c -> c.Substring(0, 1).ToUpper() + "M"
        | Generator c -> c.Substring(0, 1).ToUpper() + "G"

    map
    |> Map.toList
    |> List.sortDescending
    |> List.map (fun (f, items) ->
        let allItems =
            index
            |> List.map (function
                | Generator g when List.contains g items.Generators -> toString (Generator g)
                | Chip c when List.contains c items.Chips -> toString (Chip c)
                | _ -> ". ")
            |> (fun l -> System.String.Join(" ", l))
        let elevator = if f = floor then 'E' else '.'
        $"F%d{f} %c{elevator} %s{allItems}")
    |> (fun l -> System.String.Join("\n", l))
    |> printfn "%s"

let allMoves floor map =
    // printfn "Getting moves"
    // printMap (floor, map)

    [
        if floor < 4 then
            yield! moveWith1item floor (floor + 1) map
            yield! moveWith2items floor (floor + 1) map

        // go down
        if floor > 1 then
            yield! moveWith1item floor (floor - 1) map
            yield! moveWith2items floor (floor - 1) map

    ]
    // |> Array.filter (snd >> isValid)
    |> fun s ->
        // printfn "Moves: "
        // Seq.iteri (fun i m ->
        // printfn $"Option %d{i}:"
        // printMap m) s
        s


let isDone map =
    [1..3]
    |> List.forall (fun floor -> map |> Map.find floor |> fun f -> f.Chips.IsEmpty && f.Generators.IsEmpty)

let score steps map =
    // (0.0, map)
    // ||> Map.fold (fun score floor items ->
    //     score + float ((items.Chips.Length + items.Generators.Length) * floor * floor * floor)
    // )
    // |> (fun s -> (1000.0 - s) / 1000.0 + float steps*0.2)

    // float steps
    map
    |> Map.toList
    |> List.map (fun (floor, items) -> (4-floor) * (items.Generators.Length + items.Chips.Length))
    |> List.sum
    |> fun sum -> float steps + (float sum / 10.0)

let part1 map =
    let visited = HashSet<int * Map<int, Floor>>()
    let toVisit = PriorityQueue.empty<float * int * int * Map<int, Floor>> false

    let rec visitAll (toVisit: IPriorityQueue<float * int * int * Map<int, Floor>>)=
        printfn "%d" (toVisit.Count)
        if toVisit.IsEmpty then None else
        let (sss, steps, floor, map), toVisit = toVisit.Pop()

        match (floor, map) |> visited.Contains with
        | true -> visitAll toVisit
        | _ ->
            visited.Add(floor, map) |> ignore

            // printfn "steps: %d, score: %f" steps sss
            // printMap(floor, map)
            // System.Console.ReadKey() |> ignore

            if isDone map then
                printfn "%d" steps
                Some steps
                // Some steps
            else
                (toVisit, allMoves floor map |> List.filter (snd >> isValid))
                ||> List.fold (fun toVisit (floor, map) ->
                    match (floor, map) |> visited.Contains with
                    | true -> toVisit
                    | _ -> toVisit.Insert(score (steps + 1) map, steps + 1, floor, map) )
                |> visitAll


        // visitAll toVisit

    toVisit
    |> PriorityQueue.insert (0.0, 0, 1, map)
    |> visitAll

let part2 map =
    let firstFloorItems = Map.find 1 map

    map
    |> Map.add 1 ({ firstFloorItems with
                    Chips = "elerium"::"dilithium"::firstFloorItems.Chips |> List.sort
                    Generators = "elerium"::"dilithium"::firstFloorItems.Generators |> List.sort })
    |> part1


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // input |> parse |> part1 |> printfn "%A"
    input |> parse |> part1 |> printfn "%A"
    
    // input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
