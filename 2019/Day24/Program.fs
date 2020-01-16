open System
open System.Collections.Generic

type Cell =
    | Alive
    | Dead

let convert col = 
    match col with 
    | '.' -> Dead
    | '#' -> Alive
    | _ -> failwith "unknown"

let print cell =
    match cell with
    | Some Alive -> "#"
    | _ -> "."

let parse (input: string[]) = 
    input 
    |> Array.fold (fun (r, map) row -> 
        let _, map = 
            Seq.fold (fun (c, map) col -> 
                (c + 1, Map.add (r, c) (convert col) map)
            ) (0, map) row
        (r + 1, map)
    ) (0, Map.empty)
    |> snd

let emptyMap = 
    [for r in 0..4 do for c in 0..4 -> (r, c)]
    |> List.fold (fun map (r, c) -> Map.add (r, c) Dead map) Map.empty

let drawMap map =
    let l = Map.toList map
    let maxX = List.map (fst >> fst) l |> List.max
    let maxY = List.map (fst >> snd) l |> List.max

    //Console.SetCursorPosition(0,0)
    [for x in 0..maxX ->
        [for y in 0..maxY -> map.TryFind (x, y) |> print] |> String.concat "" 
    ] |> String.concat "\n" |> printfn "%s"
    //Threading.Thread.Sleep(200)
    //Console.ReadKey() |> ignore

let drawHyperMap hypermap =
    hypermap
    |> Map.toList 
    |> List.sort
    |> List.iter (fun (l, map) -> 
        printfn "level %d" l
        drawMap map
    )

let neighbours (r, c) =
    [(r - 1, c); (r, c - 1); (r, c + 1); (r + 1, c)]

let hyperNeighbours level rc =
    let r, c = rc
    let append n = List.map (fun c -> level + 1, c) >> List.append n

    // add neighbours on same level
    let n = neighbours rc |> List.map (fun c -> (level, c))
    // neighbours form next level
    let n =
        if rc   = (1, 2) then [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4)] |> append n
        elif rc = (3, 2) then [(4, 0); (4, 1); (4, 2); (4, 3); (4, 4)] |> append n
        elif rc = (2, 1) then [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0)] |> append n
        elif rc = (2, 3) then [(0, 4); (1, 4); (2, 4); (3, 4); (4, 4)] |> append n
        else n
    // neighbours from previous level
    let n =
        if r = 0 then (level - 1, (1, 2))::n 
        elif r = 4 then (level - 1, (3, 2))::n
        else n
    let n =
        if c = 0 then (level - 1, (2, 1))::n
        elif c = 4 then (level - 1, (2, 3))::n
        else n

    List.filter (snd >> (<>)(2, 2)) n

let count cells =
    cells
    |> Seq.filter ((=)Alive)
    |> Seq.length

let generation neighbours map =
    map
    |> Map.map (fun c cell -> 
        match cell, neighbours c with
        | Alive, 1 -> Alive
        | Alive, _ -> Dead
        | Dead, 1 | Dead, 2 -> Alive
        | cell, _ -> cell
    )

let alive map n = 
    match Map.tryFind n map with
    | Some c -> c
    | None -> Dead

let cells map neighbours =
    List.map (alive map) neighbours

let hypercells hypermap neighbours = 
    neighbours
    |> List.map (fun (l, c) -> 
        match Map.tryFind l hypermap with
        | Some m -> alive m c
        | None -> Dead)

let simulate map =
    let rec doSimulate map (visited:HashSet<Map<int*int,Cell>>) =
        let nextGeneration = generation (neighbours >> cells map >> count) map
        drawMap nextGeneration
        if visited.Contains(nextGeneration) then nextGeneration
        else 
            visited.Add(nextGeneration) |> ignore
            doSimulate nextGeneration visited
    let visited = HashSet<Map<int*int,Cell>>()
    visited.Add(map) |> ignore
    doSimulate map visited

let rec hyperSimulate n (hypermap: Map<int, Map<(int*int), Cell>>) =
    if n = 0 then hypermap else

    let addLevel fmin condition nextLevel hypermap =
        let level = Map.toList hypermap |> List.map fst |> fmin
        if Map.find level hypermap
            |> Map.toList
            |> List.filter (snd >> (=)Alive)
            |> List.exists (fst >> condition)
        then Map.add (nextLevel level) emptyMap hypermap
        else hypermap
        
    hypermap
    |> addLevel List.min (fun (r, c) -> r = 0 || r = 4 || c = 0 || c = 4) ((+)(-1)) 
    |> addLevel List.max (fun c -> ([(1, 2); (3, 2); (2, 1); (2, 3)] |> List.contains c) ) ((+)1) 
    |> Map.map (fun l map -> 
            generation (hyperNeighbours l >> hypercells hypermap >> count ) map
            |> Map.add (2, 2) Dead
        ) 
    |> hyperSimulate (n - 1)


let biodiversity map =
    map
    |> Map.toList
    |> List.map snd
    |> List.indexed
    |> List.filter (snd>>(=)Alive)
    |> List.sumBy (fun (i, _) -> Math.Pow(float 2, float i) |> int)

let countLive hypermap =
    hypermap
    |> Map.fold (fun cnt _ map -> 
        Map.fold (fun cnt _ v -> if v = Alive then cnt + 1 else cnt) cnt map
    ) 0

[<EntryPoint>]
let main argv =
    let map = IO.File.ReadAllLines("input.txt") |> parse
    // part 1
    // simulate map 
    // |> biodiversity
    // |> printfn "%d"

    // part 2
    let multiMap = 
        Map.empty
        |> Map.add 0 map
    hyperSimulate 200 multiMap 
    // |> drawHyperMap
    |> countLive
    |> printfn "%d"

    0