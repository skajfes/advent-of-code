type Direction = Right | Left
type Heading = North | South | East | West

let direction (s:string) =
    match s.[0] with
    | 'R' -> (Right, int s.[1..])
    | 'L' -> (Left, int s.[1..])
    | _ -> failwith "invalid direction"
    
let toCoordinates (heading, vert, hori) (dir, len) = 
    match heading, dir with
    | East, Left -> (North, vert + len, hori)
    | East, Right -> (South, vert - len, hori)
    | North, Left -> (West, vert, hori - len)
    | North, Right -> (East, vert, hori + len)
    | South, Left -> (East, vert, hori + len)
    | South, Right -> (West, vert, hori - len)
    | West, Left -> (South, vert - len, hori)
    | West, Right -> (North, vert + len, hori)
        
let distance (input: string) =
    input.Split(',')
    |> Seq.map (fun c -> c.Trim())
    |> Seq.map direction
    |> Seq.fold (toCoordinates) (North, 0, 0)
    |> (fun (_, a, b) -> System.Math.Abs(a) + System.Math.Abs(b))

let allVisited (_, v, h) head len =
    [1..len]
    |> List.map (fun l ->
        match head with
        | North -> (v + l, h)
        | South -> (v - l, h)
        | East -> (v, h + l)
        | West -> (v, h - l))
    
let hqLocation (input: string) =
    input.Split(',')
    |> Seq.map (fun c -> c.Trim())
    |> Seq.map direction
    |> Seq.fold (fun (s, visited) dir ->
        match s with
        | Some s ->
            let (heading, v, h) = toCoordinates s dir
            let all = allVisited s heading (snd dir)
            if List.exists (fun e -> List.contains e visited) all
            then (None, List.where (fun e -> List.contains e visited) all )
            else (Some (heading, v, h), List.append all visited)
        | None -> (s, visited)
        ) (Some (North, 0, 0), [])
    |> (fun (_, (a, b)::_) -> System.Math.Abs(a) + System.Math.Abs(b))
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllText("input.txt")
    input |> distance |> printfn "Part 1: %d"
    input |> hqLocation |> printfn "Part 2: %d"
    0 // return an integer exit code