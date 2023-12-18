open System.IO

let parse (input: string[]) = 
    input
    |> Array.mapi (fun r row ->
        row
        |> Seq.mapi (fun c col -> (r, c), col))
    |> Seq.concat
    |> Seq.toArray
    |> Array.filter (snd >> (<>)'.')

type Direction = Up | Down | Left | Right

let next (r, c) dir =
    match dir with
    | Right -> r, c + 1
    | Left -> r, c - 1
    | Up -> r - 1, c
    | Down -> r + 1, c

let rec follow_light len map todo visited visited' =
    match todo with
    | [] -> visited
    | (coord: int * int, dir: Direction)::todo ->

    if Set.contains (coord, dir) visited' then follow_light len map todo visited visited' else

    let visited' = Set.add (coord, dir) visited'
    // printfn "%A %d" (coord, dir) (Set.count visited)
    // out of bounds
    match coord with
    | r, c when r < 0 || c < 0 -> follow_light len map todo visited visited'
    | r, c when r >= len || c >= len -> follow_light len map todo visited visited'
    | _ ->

    let todo =
        match Array.tryFind (fst >> (=)coord) map with
        | None -> (next coord dir, dir)::todo
        | Some (_, m) ->
            match dir, m with
            | Right, '-' -> (next coord dir, dir)::todo
            | Left, '-' -> (next coord dir, dir)::todo
            | Up, '|' -> (next coord dir, dir)::todo
            | Down, '|' -> (next coord dir, dir)::todo
            | Right, '/' -> (next coord Up, Up)::todo
            | Right, '\\' -> (next coord Down, Down)::todo
            | Left, '/' -> (next coord Down, Down)::todo
            | Left, '\\' -> (next coord Up, Up)::todo
            | Up, '/' -> (next coord Right, Right)::todo
            | Up, '\\' -> (next coord Left, Left)::todo
            | Down, '/' -> (next coord Left, Left)::todo
            | Down, '\\' -> (next coord Right, Right)::todo
            | Right, '|' -> (next coord Up, Up)::(next coord Down, Down)::todo
            | Left, '|' -> (next coord Up, Up)::(next coord Down, Down)::todo
            | Up, '-' -> (next coord Left, Left)::(next coord Right, Right)::todo
            | Down, '-' -> (next coord Left, Left)::(next coord Right, Right)::todo
            | _ -> failwith "no"

    follow_light len map todo (Set.add coord visited) visited'


let part1 len map =
    follow_light len map [(0,0), Right] Set.empty Set.empty
    |> Set.count

let part2 len map =
    List.map (fun x -> (x, 0), Right) [0..len-1]
    |> List.append (List.map (fun x -> (x, len-1), Left) [0..len-1])
    |> List.append (List.map (fun x -> (0, x), Down) [0..len-1])
    |> List.append (List.map (fun x -> (len-1, x), Up) [0..len-1])
    |> List.map (fun start -> follow_light len map [start] Set.empty Set.empty |> Set.count)
    |> List.max

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 10 |> printfn "%A"
    testInput |> parse |> part2 10 |> printfn "%A"

    input |> parse |> part1 110 |> printfn "Part 1: %A"
    input |> parse |> part2 110 |> printfn "Part 2: %A"

    0 // return an integer exit code
