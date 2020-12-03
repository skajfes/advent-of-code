open System
let testInput =
    """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

type Field = Tree | Open

let toMap (input: string) =
    input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (Seq.map (function
            | '#' -> Tree
            | '.' -> Open
            | c -> failwithf "unknown character %c" c)
        >> Seq.toArray)

let countTrees (dr, dc) map =
    let cols = map |> Seq.head |> Seq.length
    
    let rec count (r, c) =
        match r with
        | r when r >= Seq.length map -> 0 // done
        | _ ->
            let isTree =
                if map |> Seq.item r |> Seq.item (c % cols) = Tree
                then 1 else 0 
            isTree + count (r + dr, c + dc)
    count (0, 0)
 
let checkAllSlopes slopes map =
    slopes
    |> List.map (fun s -> countTrees s map)
    |> List.fold (*) 1
    
[<EntryPoint>]
let main argv =
    let map = System.IO.File.ReadAllText("input.txt") |> toMap
    
    map |> countTrees (1, 3) |> printfn "Part 1: %A"
    map |> checkAllSlopes [(1, 1); (1, 3); (1, 5); (1, 7); (2, 1)] |> printfn "Part 2: %A"
    
    0 // return an integer exit code