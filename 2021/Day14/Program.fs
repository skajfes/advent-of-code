open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

let parse (input: string[]) =
    (("", []), input)
    ||> Array.fold (fun (template, replacements) row ->
        match row with
        | Regex @"(\w)(\w) -> (\w)" [ a; b; c ] -> template, ((char a, char b), char c)::replacements
        | Regex @"(\w+)" [template] -> template, replacements
        | _ -> template, replacements
    )

let addMap k v map =
    match Map.tryFind k map with
    | Some vv -> map |> Map.add k (v + vv)
    | None -> map |> Map.add k v

let expandPolymers steps (template: string, replacements: ((char * char) * char) list) =
    let visited = Dictionary<int*char*char,Map<char,int64>>()

    let rec insertionStep step (a: char) (b: char) =
        match step, visited.ContainsKey(step, a, b) with
        | 0, _ -> Map.empty
        | step, true -> visited[step, a, b]
        | step, false ->
            let m =
                match replacements |> List.tryFind (fst>>(=)(a, b)) with
                | None ->
                    Map.empty
                    |> addMap b 1L
                | Some (_, c) ->
                    let submap1 = insertionStep (step - 1) a c
                    let submap2 = insertionStep (step - 1) c b

                    (submap1, submap2)
                    ||> Map.fold (fun submap k v -> submap |> addMap k v)
                    |> addMap c 1

            visited[(step,a, b)] <- m
            m

    let startMap =
        template
        |> Seq.countBy id
        |> Seq.map (fun (a, b) -> a, int64 b)
        |> Map.ofSeq

    let map =
        template
        |> Seq.pairwise
        |> Seq.fold (fun map (a, b) ->
            insertionStep steps a b
            |> Map.fold (fun map k v -> addMap k v map) map
            ) startMap
        |> Map.toList
        |> List.map snd

    let max = map |> Seq.max
    let min = map |> Seq.min
    max-min

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 40 |> printfn "%A"

    input |> parse |> expandPolymers 10 |> printfn "Part 1: %d"
    input |> parse |> expandPolymers 40 |> printfn "Part 2: %d"

    0 // return an integer exit code
