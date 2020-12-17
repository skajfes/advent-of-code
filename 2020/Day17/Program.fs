let testInput =
    """.#.
..#
###"""

let parse (input: string) =
    input.Split("\r\n")
    |> Seq.mapi (fun r ->
        Seq.mapi (fun c ->
            function
            | '#' -> Some (r, c, 0)
            | _ -> None
            ))
    |> Seq.concat
    |> Seq.filter ((<>)None)
    |> Seq.map (function | Some x -> x | _ -> failwith "nope")
    |> Set.ofSeq

let mapTo4 map =
    Set.map (fun (x, y, z) -> (x, y, z, 0)) map
    
let neighbour_coords (x, y, z) =
    [for x' in x-1..x+1 do
         for y' in y-1..y+1 do
             for z' in z-1..z+1 do
                 if (x,y,z)<>(x',y',z') then yield (x', y', z')]

let neighbour_coords4 (x, y, z, w) =
    [for x' in x-1..x+1 do
         for y' in y-1..y+1 do
             for z' in z-1..z+1 do
                 for w' in w-1..w+1 do
                     if (x,y,z,w)<>(x',y',z',w') then yield (x', y', z', w')]
    
let round generator alive =
    let ns = 
        alive |> Set.map (fun coord -> coord, generator coord)
    
    // map all active cells
    let alive' =
        ns
        |> Set.map (fun (coord, ns) -> coord, ns |> Set.ofList |> Set.intersect alive |> Set.count)
        |> Set.filter (fun (_, cnt) -> cnt = 2 || cnt = 3 )
        |> Set.map fst
        
    // map all inactive cells
    ns
    |> Set.map snd
    |> List.concat
    |> List.countBy id
    |> List.filter (fun (_, cnt) -> cnt = 3)
    |> List.map fst
    |> Set.ofList
    |> fun x -> Set.difference x alive
    |> Set.union alive'
        
let bootUp generator map = 
    map
    |> round generator
    |> round generator
    |> round generator
    |> round generator
    |> round generator
    |> round generator
    
let printMap alive = 
    let zs =  Set.map (fun (_, _, z) -> z) alive
    let minz = Set.minElement zs
    let maxz = Set.maxElement zs
        
    [minz..maxz]
    |> List.iter (fun z ->
        printfn "\nZ layer: %d" z
        alive
        |> Set.filter (fun (_, _, z') -> z = z')
        |> Set.map (fun (x, y, _) -> x, y)
        |> printfn "%A" )
        
    alive
    
let printMap4 alive =
    let ws = Set.map (fun (_, _, _, w) -> w) alive
    let minw = Set.minElement ws
    let maxw = Set.maxElement ws
    
    [minw..maxw]
    |> List.iter (fun w ->
        printfn "\nW layer: %d" w
        alive
        |> Set.filter (fun (_, _, _, w') -> w' = w)
        |> Set.map (fun (x, y, z, _) -> x, y, z)
        |> printMap
        |> ignore)
    alive
        
[<EntryPoint>]
let main argv =
    let alive = System.IO.File.ReadAllText("input.txt") |> parse
    // let alive = parse testInput
    printMap alive |> ignore
    
    alive
    |> bootUp neighbour_coords
    // |> printMap
    |> Set.count |> printfn "Part 1: %A"
    
    mapTo4 alive
    |> bootUp neighbour_coords4
    // |> printMap4
    |> Set.count |> printfn "Part 2: %A"
 
    0 // return an integer exit code