open System.IO


let parse (input: string[]) =
    let parseLine (rules, pages) (input: string) =
        if input.Contains('|') then
            input.Split('|') |> function [| a; b |] -> (int a, int b)::rules, pages | _ -> failwith "nope"
        else if input.Contains(',') then
            rules, (input.Split(',') |> Array.map int) :: pages
        else rules, pages

    input
    |> Array.fold parseLine ([], [])

let valid_order (rules: (int*int)list) (pages: int array) =
    rules
    |> List.map (fun (a, b) -> Array.tryFindIndex ((=)a) pages, Array.tryFindIndex ((=)b) pages)
    |> List.filter (fun (a, b) -> a <> None && b <> None)
    |> List.forall (fun (Some a, Some b) -> a < b)

let fix_order (rules: (int*int)list) (pages: int array) =
    let r = rules
            |> List.filter (fun (a, b) -> Array.contains a pages && Array.contains b pages)
            |> List.groupBy fst |> List.map (fun (a, b) -> a, List.length b)

    pages
    |> Array.map (fun e ->
        match List.tryFind (fst >> (=)e) r with
        | Some (e, c) -> e, c
        | None -> e, 0)
    |> Array.sortBy snd
    |> Array.map fst

let sumMiddlePart (list: int array list) =
    list
    |> List.map (fun a -> a[a.Length/2])
    |> List.sum

let part1 (rules, pages) =
    pages
    |> List.filter (valid_order rules)
    |> sumMiddlePart

let part2 (rules, pages) =
    pages
    |> List.filter (valid_order rules >> not)
    |> List.map (fix_order rules)
    |> sumMiddlePart


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
