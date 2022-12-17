open System.IO
open FParsec

type Item =
    | Item of int
    | List of Item list

let pItem, pItemRef = createParserForwardedToRef<Item, unit> ()
let pList = between (pstring "[") (pstring "]") (sepBy pItem (pstring ","))
pItemRef.Value <- (pList |>> List) <|> (pint32 |>> Item)

let parseLine (input: string) =
    match run pList input with
    | Success (items, _, _) -> items
    | _ -> failwith "unable to parse input"

let parse (input: string[]) =
    input
    |> Array.filter (fun x -> x <> "")
    |> Array.map parseLine

let rec compareItems (left, right) =
    match left, right with
    | Item l :: _, Item r :: _ when l < r -> Some true
    | Item l :: _, Item r :: _ when l > r -> Some false
    | Item l :: left, Item r :: right -> compareItems (left, right)
    | [], Item _ :: _ -> Some true
    | [], List _ :: _ -> Some true
    | Item _ :: _, [] -> Some false
    | List _ :: _, [] -> Some false
    | [], [] -> None
    | Item l :: left, List r :: right ->
        match compareItems ([ Item l ], r) with
        | Some r -> Some r
        | None -> compareItems (left, right)
    | List l :: left, Item r :: right ->
        match compareItems (l, [ Item r ]) with
        | Some r -> Some r
        | None -> compareItems (left, right)
    | List l :: left, List r :: right ->
        match compareItems (l, r) with
        | Some r -> Some r
        | None -> compareItems (left, right)

let part1 packets =
    packets
    |> Array.chunkBySize 2
    |> Array.map (fun [| l; r |] -> (l, r))
    |> Array.map compareItems
    |> Array.indexed
    |> Array.map (fun (i, r) -> i + 1, r)
    |> Array.filter (snd >> (=)(Some true))
    |> Array.sumBy fst

let part2 (packets: Item list[]) =
    packets
    |> Array.append [| parseLine "[[2]]"; parseLine "[[6]]" |]
    |> Array.sortWith (fun a b -> match compareItems (a, b) with
                                   | None -> 0
                                   | Some true -> -1
                                   | Some false -> 1)
    |> Array.indexed
    |> Array.map (fun (i, e) -> i+1, e)
    |> Array.filter (fun (i, e) -> e = parseLine "[[2]]" || e = parseLine "[[6]]")
    |> Array.fold (fun s (i, e) -> s * i) 1

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
