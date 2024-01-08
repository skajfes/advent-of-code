open System.Collections.Generic
open System.Diagnostics
open System.IO

let parse (input: string[]) =
    let map =
        input
        |> Array.mapi (fun r row ->
            row
            |> Seq.mapi (fun c col ->  (r, c), col))
        |> Seq.concat
        |> Seq.filter (fun (_, v) -> Seq.contains v ".^>v<")
        |> Map.ofSeq
    let start = map |> Map.keys |> Seq.minBy fst
    let stop = map |> Map.keys |> Seq.maxBy fst
    map, start, stop

let neighbours map ((r, c), v) =
    match v with
    | '.' -> [| (r-1, c); (r+1, c); (r, c-1); (r, c+1) |]
    | '>' -> [| (r, c+1) |]
    | '<' -> [| (r, c-1) |]
    | '^' -> [| (r-1, c) |]
    | 'v' -> [| (r+1, c) |]
    | _ -> failwith "no"
    |> Array.filter (fun p -> Map.containsKey p map)
    |> Array.map (fun p -> p, Map.find p map)

let neighbours2 map ((r, c), _) =
    [| (r-1, c); (r+1, c); (r, c-1); (r, c+1) |]
    |> Array.filter (fun p -> Map.containsKey p map)
    |> Array.map (fun p -> p, Map.find p map)


let rec follow neighbours map stop node path =
    neighbours map node
    |> Array.filter (fun i -> path |> List.contains i |> not)
    |> function
        | [| |] -> None
        | [| x |] when fst x = stop -> Some (1, x)
        | [| x |] -> match follow neighbours map stop x (node::path) with
                     | None -> None
                     | Some (len, e) -> Some (1 + len, e)
        | _ -> Some (0, node)

let coord_map = List<int*int>()
let mmm c =
    let idx = coord_map.IndexOf(c)
    if idx >= 0 then idx
    else
        coord_map.Add(c)
        coord_map.IndexOf(c)

let add_path (path: int64) node =
    // path[node] <- true
    // path |> Array.mapi (fun i v -> if i = node then true else v)
    path ||| (1L <<< node)
let in_path (path: int64) node = path &&& (1L <<< node) > 0

let add a b len graph =
    let ns =
        match Map.tryFind (mmm a) graph with
        | Some ns -> ns
        | None -> []
    if List.contains (len, mmm b) ns then graph
    else graph |> Map.add (mmm a) ((len, mmm b)::ns)

let to_graph neighbours map start stop two_way =
    let q = Queue<int*int>()
    let visited = HashSet<int*int>()

    let rec go (todo:Queue<int*int>) (graph: Map<int, (int * int) list>) =
        if todo.Count = 0 then graph else
        let node = todo.Dequeue()
        if visited.Contains(node) then go todo graph else

        visited.Add(node) |> ignore
        let n = map |> Map.find node

        neighbours map (node, n)
        |> Array.map (fun nn -> follow neighbours map stop nn [node, n])
        |> Array.fold (fun graph ns ->
            ns
            |> function
                | None -> graph
                | Some (len, n) ->
                    // add next to process
                    todo.Enqueue(fst n)

                    graph
                    |> add node (fst n) (len + 1) // add path a to b
                    |> fun g ->
                        if two_way then add (fst n) node (len + 1) g else g // add path b to a

                ) graph
        |> go todo

    q.Enqueue(start)
    go q Map.empty
    |> Map.add (mmm stop) [] // add the stop point to graph
    |> Map.map (fun _ v -> v |> List.toArray)

let findAllPaths start stop graph =
    let todo = Stack<struct (int*int64*int)>()
    todo.Push(start, 0L, 0)

    let rec find max_length =
        if todo.Count = 0 then max_length else
        match todo.Pop() with
        | p, _, len when p = stop ->
            find (max len max_length)
        | pos, path, len ->
            graph |> Map.find pos
            |> Array.iter (fun (l, p) ->
                if in_path path p = false then
                    todo.Push(p, add_path path p, l + len))

            find max_length

    find 0


let part1 (map, start, stop) =
    to_graph neighbours map start stop false
    |> findAllPaths (mmm start) (mmm stop)

let part2 (map, start, stop) =
    to_graph neighbours2 map start stop true
    |> findAllPaths (mmm start) (mmm stop)

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    let timer = Stopwatch.StartNew()
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"
    timer.Stop()
    printfn "elapsed %A" timer.Elapsed

    0 // return an integer exit code
