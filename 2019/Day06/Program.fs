open System

type Tree =
    | Leaf of (string * int)
    | Node of (string * int) * Tree seq

let node n d = Leaf (n, d)
let empty = node "COM" 0

let rec fold f agg tree =
    match tree with
    | Leaf l -> f agg l
    | Node (l, cs) -> 
        let agg' = f agg l
        Seq.fold (fold f) agg' cs

let rec fillTree orbits tree =
    let makeChildren (n, d) =
        orbits
        |> Seq.filter (fun o -> fst o = n) 
        |> Seq.map (fun o -> node (snd o) (d + 1) |> fillTree orbits)
        
    match tree with 
    | Leaf l -> Node (l, makeChildren l)
    | Node (l, ch) -> Node (l, Seq.append ch (makeChildren l))
        
let parse (input: string) =
    input.Split('\n')
    |> Seq.map (fun (x:string) -> 
                    let r = x.Split(')')
                    (r.[0], r.[1]))
    |> (fun o -> fillTree o empty)
    // |> Seq.fold (fun t (p, c) -> add p c t) empty

let rec find target tree =
    match tree with
    | Leaf (n, d) when n = target -> [(n, d)]
    | Node ((n, d), _) when n = target -> [(n, d)]
    | Node (o, ch) -> 
        match Seq.map (find target) ch 
            |> Seq.filter (List.isEmpty >> not) 
            |> Seq.concat 
            |> Seq.toList with
        | [] -> []
        | xs -> o :: xs
    | _ -> []

[<EntryPoint>]
let main argv =

    let input = System.IO.File.ReadAllText("input.txt")
//     let input = "COM)B
// B)C
// C)D
// D)E
// E)F
// B)G
// G)H
// D)I
// E)J
// J)K
// K)L
// K)YOU
// I)SAN"
    let orbits = parse input

    // part 1
    let totalOrbits = fold (fun t l -> t + (snd l)) 0 orbits

    // part 2
    let you = find "YOU" orbits
    let san = find "SAN" orbits

    let len1 = List.skipWhile (fun e -> List.contains e san) you |> List.length
    let len2 = List.skipWhile (fun e -> List.contains e you) san |> List.length
    len1 + len2 - 2
    //0
    //you