// #load "Heap.fs"

open System
open System.Collections.Generic
open FSharpx.Collections

type Portal = 
    // | Start
    // | Stop
    | Outer of string
    | Inner of string

type Element = 
    | Wall
    | Hall
    | Portal of Portal

let drawMap map =
    let l = Map.toList map
    let maxX = List.map (fst >> fst) l |> List.max
    let maxY = List.map (fst >> snd) l |> List.max

    [for x in 0..maxX ->
        [for y in 0..maxY -> 
            match map.TryFind (x, y) with
            | Some Wall -> "#"
            | Some Hall -> "."
            | Some (Portal (Outer p)) -> string p.[0]
            | Some (Portal (Inner p)) -> string p.[0]
            | None -> " "
        ] |> String.concat "" 
    ] |> String.concat "\n" |> printfn "%s"

let parseMap input =
    Array.indexed input
    |> Array.fold (fun map (r, row) -> 
        Seq.indexed row
        |> Seq.fold (fun map (c, col) ->
            let el =
                match col with
                | ' ' | '#' -> Wall
                | '.' -> Hall
                | a -> 
                    // find full portal name and position
                    let up = if r > 0 then Seq.item c input.[r-1] else ' '
                    let down = if r < Array.length input - 1 then Seq.item c input.[r+1] else ' '
                    let left = if c > 0 then Seq.item (c-1) row else ' '
                    let right = if c < String.length row - 1 then Seq.item (c+1) row else ' '
                    let name = 
                        if up = '.' then string a + string down
                        elif down = '.' then string up + string a
                        elif left = '.' then string a + string right
                        elif right = '.' then string left + string a
                        else ""
                    if name <> "" then 
                        // inner or outer portal?
                        let isOuter = r < 2 || c < 2 || r >= Array.length input - 3 || c >= String.length row - 3
                        if isOuter 
                        then Portal (Outer name)
                        else Portal (Inner name)
                    else Wall
            Map.add (r, c) el map
        ) map
    ) Map.empty

let getPortal (portal: Portal) (map: Map<int*int,Element>) =
    map
    |> Map.toList
    |> List.filter (fun (_, e) -> e = Portal (portal))
    |> List.map fst
    |> List.tryHead

type Direction =
    | Up
    | Down
    | Left
    | Right

let move player direction =
    let r, c = player
    match direction with
    | Up -> (r - 1, c)
    | Down -> (r + 1, c)
    | Left -> (r, c - 1)
    | Right -> (r, c + 1)

type Distance = Set<Portal> * int

let opposite =
    function
    | Outer p -> Inner p
    | Inner p -> Outer p

let findDistances map: Set<Distance> =
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
                | Some (Portal (p)) -> 
                    // we don't count the portals as steps, so don't include starting and ending point in length
                    Set.add (set [prevNode; p], count - 1) edges 
                | Some Wall | None -> edges
        ) (Set.empty)

    let rec scanAll position (node: Portal) (edges: Set<Distance>) (visitedNodes: Set<Portal>) =
        let d1 = scan position node 0 Set.empty
        let visitedNodes = Set.add node visitedNodes
        
        // printfn "start: %A, %A" node position
        // printfn "visited: %A" visitedNodes
        // Console.ReadKey() |> ignore 

        d1
        |> Seq.collect fst
        |> Seq.map opposite
        |> Seq.filter (fun n -> Set.contains n visitedNodes |> not)
        |> Seq.fold (fun (edges, visitedNodes) portal ->
            match getPortal portal map with
            | Some pos -> scanAll pos portal edges visitedNodes
            | None -> (edges, visitedNodes)
           ) (Set.union edges d1, visitedNodes)

    let start = Outer("AA")
    match getPortal start map with
    | Some pos -> scanAll pos start Set.empty Set.empty |> fst
    | _ -> failwith "wrong"

let dimensionPortal (level, portal) =
    match portal with
    | Outer p -> (level - 1, Inner p)
    | Inner p -> (level + 1, Outer p)

let rec visitNodeDijstra nodes distances (visited: HashSet<int*Portal>) =

    let (currentDist, currentPortal), nodes = PriorityQueue.pop nodes

    if visited.Contains(currentPortal)
    then visitNodeDijstra nodes distances visited 
    else

    if snd currentPortal = Outer "ZZ" then currentDist else

    // find all neighbours
    let neighbours =
        distances
        |> Set.filter (fst >> Set.contains (snd currentPortal))
        |> Set.map (fun (nodes, d) -> (nodes |> Set.remove (snd currentPortal) |> Seq.head, d))
        |> Set.map (fun (portal, d) -> (fst currentPortal, portal), d + currentDist)
        |> Set.add (dimensionPortal currentPortal, 1 + currentDist)
        |> Set.filter (fun ((level, portal), _) -> if portal = Outer "AA" || portal = Outer "ZZ" then level = 0 else true)
        |> Set.filter (fun ((level, _), _) -> level >= 0)
        |> Set.filter (fun (el, _) -> visited.Contains (el) |> not)

    // add to nodes
    let nodes =
        neighbours
        |> Set.fold (fun nodes (portal, d) -> 
                PriorityQueue.insert (d, portal) nodes
        ) nodes

    // printfn "current: %A %d" currentPortal currentDist
    // printfn "  neighbours: %A" neighbours
    // printfn "  nodes: %A" nodes
    // Console.ReadKey() |> ignore 

    visited.Add(currentPortal) |> ignore
    visitNodeDijstra nodes distances visited 

let visitAllDijsktra distances =
    let start = Outer "AA"
    let nodes =
        PriorityQueue.empty false
        |> PriorityQueue.insert (0, (0, start))

    let visited = HashSet<int * Portal>()
    visitNodeDijstra nodes distances visited 



[<EntryPoint>]
let main argv =
    let map = IO.File.ReadAllLines("input.txt") |> parseMap
    drawMap map
    let distances = findDistances map
    visitAllDijsktra distances


