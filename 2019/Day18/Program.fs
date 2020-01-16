open System

// #load "Heap.fs"

type Element =
    | Wall
    | Hall
    | Key of char
    | Door of char
    | Start

type Direction =
    | Up
    | Down
    | Left
    | Right

let parse (input:string[]) =
    Seq.mapi (fun r row ->
        Seq.mapi (fun c e ->
            ((r, c), match e with
                     | '#' -> Wall
                     | '.' -> Hall
                     | k when Char.IsLower(k) -> Key k
                     | k when Char.IsUpper(k) -> Door (Char.ToLower(k))
                     | '@' -> Start
                     | _ -> failwith "unknown char")
        ) row
    ) input
    |> Seq.concat
    |> Map.ofSeq

let drawMap map =
    let l = Map.toList map
    let minX = List.map (fst >> fst) l |> List.min
    let minY = List.map (fst >> snd) l |> List.min
    let maxX = List.map (fst >> fst) l |> List.max
    let maxY = List.map (fst >> snd) l |> List.max
    // List.map (fun ((x, y), d) -> ((x + minX, y + minY), d)) l

    //Console.SetCursorPosition(0,0)
    [for x in minX..maxX ->
        [for y in minY..maxY ->
            match map.TryFind (x, y) with
            | Some d ->
                match d with
                | Wall -> "#"
                | Hall -> "."
                | Key k -> string k
                | Door d -> string <| Char.ToUpper(d)
                | Start -> "@"
            | None -> " "
        ] |> String.concat ""
    ] |> String.concat "\n" |> printfn "%s"
    //Console.ReadKey() |> ignore

let getNode node map =
    map
    |> Map.filter (fun _ v -> v = node)
    |> Map.toList
    |> List.map fst

let move player direction =
    let r, c = player
    match direction with
    | Up -> (r - 1, c)
    | Down -> (r + 1, c)
    | Left -> (r, c - 1)
    | Right -> (r, c + 1)

type Distance = Set<Element> * int

let findDistances map: Set<Distance> list =
    let rec scan position prevNode count visited =
        let visited = Set.add position visited
        [Up;Down;Left;Right]
        |> List.fold (fun (edges) direction ->
            let position = move position direction
            if Set.contains position visited
            then edges
            else
                match Map.tryFind position map with
                | Some Hall ->
                    scan position prevNode (count + 1) visited
                    |> Set.union edges
                | Some (Key k) -> Set.add (set [prevNode; Key k], count + 1) edges
                | Some (Door d) -> Set.add (set [prevNode; Door d], count + 1) edges
                | Some Start -> Set.add (set [prevNode; Start], count + 1) edges
                | Some Wall | None -> edges
        ) (Set.empty)

    let rec scanAll position node edges visitedNodes =
        let d1 = scan position node 0 Set.empty
        let visitedNodes = Set.add node visitedNodes

        d1
        |> Seq.collect fst
        |> Seq.filter (fun n -> Set.contains n visitedNodes |> not)
        |> Seq.fold (fun (edges, visitedNodes) n ->
            let pos = getNode n map |> List.head
            scanAll pos n edges visitedNodes
           ) (Set.union edges d1, visitedNodes)

    getNode Start map
    |> List.map (fun position ->
        scanAll position Start Set.empty Set.empty |> fst
    )


let haveKey keys (node, _) =
    match node with
    | Door d -> Set.contains (Key d) keys
    | _ -> true

// #load "Heap.fs"
open FSharpx.Collections
open System.Collections.Generic

let replace state idx el =
    List.mapi (fun i e -> if i = idx then el else e) state

let rec visitNodeDijstra 
    (nodes:IPriorityQueue<int*(Element list*Set<Element>)>) 
    (distances:Set<Distance> list) 
    allKeys 
    (visited:HashSet<(int * Element) * Set<Element>>) =

    let (currentDist, (currentPositions, currentKeys)), nodes =
        PriorityQueue.pop nodes

    let isvisited = currentPositions |> List.indexed |> List.forall (fun el -> visited.Contains(el, currentKeys))

    if isvisited
    then visitNodeDijstra nodes distances allKeys visited 
    else

    if currentKeys = allKeys then currentDist else

    // find all neighbours
    let neighbours =
        List.zip currentPositions distances
        |> List.mapi (fun idx (current, distances) -> 
            distances
            |> Set.filter (fst >> Set.contains current)
            |> Set.map (fun (nodes, d) -> (nodes |> Set.remove current |> Seq.head, d))
            |> Set.filter (haveKey currentKeys)
            |> Set.map (fun (el, d) -> ((idx, el), d + currentDist))
            |> Set.filter (fun (neighbourState, _) -> visited.Contains (neighbourState, currentKeys) |> not)
            |> Set.map (fun ((idx, el), d) -> (replace currentPositions idx el, d))
            )

    // add to nodes
    let nodes =
        neighbours
        |> List.fold (fun nodes neighbours -> 
            Set.fold (fun nodes (neighbourState, d) -> 
                let keys =
                    neighbourState
                    |> List.fold (fun keys el ->
                        match el with
                        | Key _ as k -> Set.add k keys
                        | _ -> keys
                    ) currentKeys 
                PriorityQueue.insert (d, (neighbourState, keys)) nodes
                //TODO: if keys = allKeys quit
            ) nodes neighbours
        ) nodes

    currentPositions
    |> List.indexed
    |> List.iter (fun el -> visited.Add(el, currentKeys) |> ignore)

    visitNodeDijstra nodes distances allKeys visited 

let visitAllDijsktra distances =
    let start = List.map (fun _ -> Start) distances 
    let nodes =
        PriorityQueue.empty false
        |> PriorityQueue.insert (0, (start, Set.empty))

    let missingKeys =
        distances
        |> Seq.concat
        |> Seq.collect fst
        |> Set.ofSeq
        |> Set.filter (fun e -> match e with | Key _ -> true | _ -> false)

    let visited = HashSet<(int * Element) * Set<Element>>()

    visitNodeDijstra nodes distances missingKeys visited 

[<EntryPoint>]
let main argv =
    // part 1
    // let map = IO.File.ReadAllLines("input.txt") |> parse
    // drawMap map
    // let distances = findDistances map
    // visitAllDijsktra distances

    // part 2
    let map = IO.File.ReadAllLines("input-part2.txt") |> parse
    drawMap map
    let distances = findDistances map
    visitAllDijsktra distances 


