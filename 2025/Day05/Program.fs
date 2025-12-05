open System.IO

let parse (input: string[]) = 
    (([], []), input)
    ||> Array.fold (fun (ranges, items) s ->
        if s.Contains("-")
        then
             let [|a; b|] = s.Split("-")
             (int64 a, int64 b)::ranges, items
        elif s.Length = 0 then ranges, items
        else ranges, (int64 s)::items
             )

let part1 (ranges, items) =
    items |> List.filter (fun i -> ranges |> Seq.exists (fun (a, b) -> a <= i && i <= b) ) |> List.length

let rec dedup (r::rs) =
    if rs = [] then [r] else
    let r' =
        (r, rs)
        ||> List.fold (
            fun (a', b') (a, b) ->
                if a' < a && b' < a then a', b' // ends before next one starts -> no change
                elif a' < a && b' >= a && b' <= b then a', a-1L // starts before, ends in other -> make it end before a-b
                elif a' = a && b' <= b then 0L,-1L // a'b' completely contained in a-b, make it add up to 0 (we add +1 later)
                else failwith "should not happen"
            )
    r'::(dedup rs)

let rec remove_contained ((a, b)::rs) =
    if rs = [] then [(a,b)] else
    match rs |> List.exists (fun (a', b') -> a >= a' && b <= b') with
    | true -> remove_contained rs
    | false -> (a, b)::(remove_contained rs)

let part2 (ranges, _) =
    ranges
    |> List.sortDescending
    |> remove_contained
    |> List.sort
    |> dedup
    |> List.sumBy (fun (a, b) -> b - a + 1L)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
