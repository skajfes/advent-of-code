open System.IO

let neighbours ((r, c), pipe) =
    let prod el = List.map (fun x -> el, x)
    let up = prod (r-1, c) ['7'; '|'; 'F']
    let down = prod (r+1, c) ['J'; '|'; 'L']
    let left = prod (r, c-1) ['F'; '-'; 'L']
    let right = prod (r, c+1) ['J'; '-'; '7']

    match pipe with
    | 'S' -> up @ down @ left @ right
    | '-' -> left @ right
    | '|' -> up @ down
    | 'L' -> up @ right
    | 'F' -> right @ down
    | 'J' -> up @ left
    | '7' -> left @ down
    | _ -> failwith "nope"


let toPath coords =
    let rec get_path (node::stack) path =
        // printfn "len %d" (List.length path)
        if Map.containsKey (fst node) path then path else
        neighbours node
        |> List.filter (fun x -> Array.contains x coords)
        |> List.filter (fun x -> Map.containsKey (fst x) path |> not)
        |> List.fold (fun stack node -> node::stack) stack
        |> fun stack -> get_path stack (Map.add (fst node) (snd node) path)

    let start = coords |> Array.find (snd >> (=)'S')
    get_path [start] Map.empty

let parse (input: string[]) = 
    input
    |> Array.mapi (fun r row -> Seq.mapi (fun c char -> (r, c), char) row |> Seq.toArray)
    |> Array.concat
    |> Array.filter (fun (x, c) -> c <> '.')
    |> toPath

let part1 path =
    Set.count path / 2

let checkRight path ymax (x, y) =
    [for y in (y + 1)..ymax -> x, y]
    |> List.filter (fun x -> Map.containsKey x path)
    |> List.map (fun x -> Map.find x path)
    |> List.fold (fun (crossed, o) pipe ->
                  match o, pipe with
                  | None, '|' -> crossed + 1, None
                  | None, 'L' -> crossed, Some 'L'
                  | None, 'F' -> crossed, Some 'F'
                  | Some 'L', '-' -> crossed, Some 'L'
                  | Some 'F', '-' -> crossed, Some 'F'
                  | Some 'L', '7' -> crossed + 1, None
                  | Some 'F', 'J' -> crossed + 1, None
                  | Some 'L', 'J' -> crossed, None
                  | Some 'F', '7' -> crossed, None
                  | None, 'S' -> crossed, Some 'F' // specific to input
                  | a -> failwithf "invalid combo %A" a
                  ) (0, None)
    // |> fun l ->
    //     printfn "%A %A" (x, y) l
    //     l
    |> fun x -> fst x % 2 = 1

let part2 og_path =
    let all_path = Map.keys og_path
    let xmin = Seq.map fst all_path |> Seq.min
    let xmax = Seq.map fst all_path |> Seq.max
    let ymin = Seq.map snd all_path |> Seq.min
    let ymax = Seq.map snd all_path |> Seq.max
    printfn "(%d, %d) (%d, %d)" xmin xmax ymin ymax
    // let path = og_path |> Set.filter (snd >> (=) '|') |> Set.map fst

    [for x in xmin..xmax do for y in ymin..ymax -> (x, y)]
    |> List.filter (fun x -> Map.containsKey x og_path |> not)
    // |> fun x ->
    //     printfn "%A" x
    //     x
    |> List.filter (checkRight og_path ymax)
    |> List.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample3.txt")
    
    testInput |> parse |> part2 |> printfn "%A"
    
    // input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
