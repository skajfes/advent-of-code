open System
open System.Collections.Generic
open System.IO

let parse (input: string[]) =
    let map =
        input
        |> Array.mapi (fun r row ->
            row
            |> Seq.mapi (fun c col -> (r, c), col))
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

let neighbours2 map ((r, c), v) =
    [| (r-1, c); (r+1, c); (r, c-1); (r, c+1) |]
    |> Array.filter (fun p -> Map.containsKey p map)
    |> Array.map (fun p -> p, Map.find p map)

let findAllPaths neighbours map start stop =

    let distances = Dictionary<(int*int)*char, int>()
    let rec find todo max_length =
        match todo with
        | [] -> max_length
        | ((p, '.'), path)::todo when p = stop ->
            printfn "found path length %d max %d q: %d" (List.length path) max_length (List.length todo)
            path |> List.rev |> List.iteri (fun i pos -> if distances.ContainsKey(pos) |> not || distances[pos] < (i+i) then distances[pos] <- i+1)
            find todo (max (List.length path) max_length)
        | (pos, path)::todo ->
            // let l = List.length path
            // if distances.ContainsKey(pos) |> not || distances[pos] < l then
                neighbours map pos
                |> Array.filter (fun p -> path |> List.contains p |> not)
                |> Array.fold (fun todo pos' -> (pos', pos'::path)::todo) todo
                |> fun todo -> find todo max_length
            // else
            //     find todo max_length

    find [(start, '.'), []] 0

let findAllPaths' neighbours map start stop =

    let distances = Dictionary<(int*int)*char, int>()
    let visited = HashSet<(int*int)*char>()

    let rec find pos length =
        if visited.Contains(pos) then () else

        visited.Add(pos) |> ignore

        if distances.ContainsKey(pos) |> not || distances[pos] < length then
            distances[pos] <- length

        // if distances[pos] > length then () else

        neighbours map pos
        |> Array.iter (fun pos' -> find pos' (length + 1))

        visited.Remove(pos) |> ignore

    find (start, '.') 0

    distances[stop, '.']


let findAllPaths'' neighbours map start stop =

    let distances = Dictionary<(int*int)*char, int>()
    let queue = PriorityQueue<((int*int)*char)*int, int>()
    let visited = HashSet<(int*int)*char>()

    let rec find (queue: PriorityQueue<((int*int)*char)*int, int>) =
        let pos, len = queue.Dequeue()

        if visited.Contains(pos) then find queue else

        if distances.ContainsKey(pos) |> not || distances[pos] < len then distances[pos]<-len

        neighbours map pos
        // |> Array.filter (fun p -> path |> List.contains p |> not)
        |> Array.fold (fun (queue: PriorityQueue<((int*int)*char)*int, int>) pos' ->
            queue.Enqueue((pos', len + 1), -(len+1))
            queue) queue
        |> find

    queue.Enqueue(((start, '.'), 0), 0)
    find queue

    distances[(stop, '.')]

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

let add a b len graph =
    let ns =
        match Map.tryFind a graph with
        | Some ns -> ns
        | None -> []
    if List.contains (len, b) ns then graph
    else graph |> Map.add a ((len, b)::ns)

let to_graph neighbours map start stop =
    let q = Queue<int*int>()
    let visited = HashSet<int*int>()

    let rec go (todo:Queue<int*int>) (graph: Map<int*int, (int * (int*int)) list>) =
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
                    |> add node (fst n) (len + 1)
                    |> add (fst n) node (len + 1)

                ) graph
        |> go todo

    q.Enqueue(start)
    go q Map.empty


let findAllPaths''' neighbours map start stop =
    let graph = to_graph neighbours map start stop |> Map.add stop []

    let rec find todo max_length =
        match todo with
        | [] -> max_length
        | (p, path)::todo when p = stop ->
            let l = path |> List.sumBy fst
            printfn "found path length %d max %d q: %d" l max_length (List.length todo)
            // path |> List.rev |> List.iteri (fun i (_, pos) -> if distances.ContainsKey(pos) |> not || distances[pos] < (i+i) then distances[pos] <- i+1)
            find todo (max l max_length)
        | (pos, path)::todo ->
            graph |> Map.find pos
            |> List.filter (fun (_, p) -> path |> List.map snd |> List.contains p |> not)
            |> List.fold (fun todo p' -> (snd p', p'::path)::todo) todo
            |> fun todo -> find todo max_length

    find [start, []] 0


let part1 (map, start, stop) =
    findAllPaths''' neighbours map start stop

let part2 (map, start, stop) =
    findAllPaths''' neighbours2 map start stop

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    // input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
