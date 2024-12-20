open System.Collections.Generic
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
    |> fun (start, stop, path) -> start, stop, HashSet(path)

let neighbours (r, c) =
    [ (r+1, c); (r-1, c); (r, c-1); (r, c+1) ]

let walk start stop (path: HashSet<int*int>) =
    let rec loop current visited =
        let next =
            neighbours current
            |> List.filter path.Contains

        let next =
            match List.tryHead visited with
            | None -> next
            | Some v -> next |> List.filter (fun c -> c <> v)

        let visited = current::visited

        match next with
        | [next] when next = stop -> next::visited |> List.rev |> List.toArray
        | [next] -> loop next visited
        | _ -> failwith "nope"

    loop start []

let by_length max_cheat path (ei, (er, ec)) =
    path
    |> Array.fold (fun cnt (i, (r, c)) ->
        if ei >= i then cnt else
        let d = abs (er - r) + abs (ec - c)
        if d > max_cheat then cnt else
        if abs (i - ei) - d >= 100
        then cnt + 1
        else cnt
    ) 0

let find_distances_saved max_cheat (start, stop, path)  =
    walk start stop path
    |> Array.indexed
    |> fun i -> i |> Array.sumBy (by_length max_cheat i)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> find_distances 2 |> printfn "%A"
    // testInput |> parse |> find_distances 20 |> printfn "%A"

    input |> parse |> find_distances_saved 2 |> printfn "Part 1: %A"
    input |> parse |> find_distances_saved 20 |> printfn "Part 2: %A"

    0 // return an integer exit code
