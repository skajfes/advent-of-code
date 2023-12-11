open System.IO

let parse (input: string[]) = 
    input
    |> Array.mapi (fun r row ->
        row.ToCharArray()
        |> Array.mapi (fun c ->
            function
            | '#' -> Some (int64 r, int64 c)
            | _ -> None
        )
    )
    |> Array.concat
    |> Array.filter (function | Some x -> true | _ -> false)
    |> Array.map (function | Some x -> x | _ -> failwith "nope")
    |> List.ofArray


let doExpand coord inc universe =
    let xmax = universe |> List.map coord |> List.max
    (List.empty, [xmax .. -1L .. 0L])
    ||> List.fold (fun u r ->
        let row = universe |> List.filter (coord >> (=) r)
        match List.length row with
        | 0 -> List.map inc u
        | _ -> List.append row u)

let expand amount universe =
    universe
    |> doExpand fst (fun (r, c) -> r + amount, c)
    |> doExpand snd (fun (r, c) -> r, c + amount)
    |> List.sort

let rec distances sum universe =
    match universe with
    | [] -> sum
    | (xg, yg)::gs ->
        let ds = gs
                 |> List.map (fun (x, y) -> abs (xg - x) + abs (yg - y) )
                 |> List.sum
        distances (sum + ds) gs

let part1 universe =
    universe
    |> expand 1L
    |> distances 0L

let part2 universe =
    universe
    |> expand 999999L
    |> distances 0L

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
