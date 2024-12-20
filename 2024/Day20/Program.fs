open System.IO

type Maze =
    | Path of (int*int)
    | Start of (int*int)
    | End of (int*int)
    | Wall

let parse (input: string[]) = 
    input
    |> Array.mapi(fun r row ->
        row
        |> Seq.mapi(fun c ->
            function
            | '.' -> Path (r, c)
            | 'S' -> Start (r, c)
            | 'E' -> End (r, c)
            | _ -> Wall)
        )
    |> Seq.concat
    |> Seq.fold (fun (start, stop, path) f ->
        match f with
        | Path foo -> start, stop, foo::path
        | Start foo -> foo, stop, foo::path
        | End foo -> start, foo, foo::path
        | Wall -> start, stop, path) ((0,0), (0,0), [])

let neighbours (r, c) =
    [ (r+1, c); (r-1, c); (r, c-1); (r, c+1) ]

let walk start stop path =
    let rec loop current visited =
        let next =
            neighbours current
            |> List.filter (fun c -> visited |> List.contains c |> not)
            |> List.filter (fun c -> List.contains c path)

        match next with
        | [next] when next = stop -> next::visited |> List.rev
        | [next] -> loop next (next::visited)
        | _ -> failwith "nope"

    loop start [start]

let by_length max_cheat path (ei, (er, ec)) =
    path
    |> List.filter (fun (i, _) -> i < ei) // look at next points in path
    |> List.map (fun (i, (r, c)) -> i, (abs (er - r) + abs (ec - c))) // calculate distance to point
    |> List.filter (fun (i, d) -> d <= max_cheat) // where distance less than max_cheat
    |> List.map (fun (i, d) -> abs (i - ei) - d) // calculate saved steps

let find_shortest length (path: (int*int)list) =
    let indexed = path |> List.indexed
    indexed
    |> List.map (by_length length indexed)
    |> List.concat
    |> List.filter ((<)0)

let find_distances length (start, stop, path)  =
    walk start stop path
    |> find_shortest length
    |> List.groupBy id
    |> List.map (fun (i, l) -> i, List.length l)
    |> List.sort

let find_distances_saved length (start, stop, path)  =
    walk start stop path
    |> find_shortest length
    |> List.filter ((<=)100)
    |> List.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> find_distances 2 |> printfn "%A"
    testInput |> parse |> find_distances 20 |> printfn "%A"

    input |> parse |> find_distances_saved 2 |> printfn "Part 1: %A"
    input |> parse |> find_distances_saved 20 |> printfn "Part 2: %A"

    0 // return an integer exit code
