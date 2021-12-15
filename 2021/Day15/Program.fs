open System.Collections.Generic
open System.IO
open FSharpx.Collections

let parse (input: string[]) = 
    input
    |> Array.map (Seq.map (string>>int))
    |> array2D

let neighbours (x, y) = [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]

let shortestPath (map: int[,]) =
    let goal = (Array2D.length1 map - 1, Array2D.length2 map - 1)
    let visited = HashSet<int * int>()
    let queue = PriorityQueue.empty false
                |> PriorityQueue.insert (0, (0, 0))

    let rec find queue  =
        let (length, coord), queue = PriorityQueue.pop queue

        if visited.Contains(coord) then find queue else

        // printfn "%d, %A %d" length coord visited.Count
        if coord = goal then length else
        visited.Add(coord) |> ignore

        let queue =
            neighbours coord
            |> List.filter (fun (x, y) -> x >= 0 && x <= fst goal && y >= 0 && y <= snd goal)
            |> List.filter (fun c -> visited.Contains(c) |> not)
            |> List.map (fun (x, y) -> length + map[x, y], (x, y))
            |> List.fold (fun queue item -> queue |> PriorityQueue.insert item) queue

        find queue
    find queue

let part1 map =
    shortestPath map

let expand map =
    let lenx = Array2D.length1 map
    let leny = Array2D.length2 map
    Array2D.init (lenx*5) (leny*5)
        (fun x y ->
            let incx = x / lenx
            let incy = y / leny
            match (map[x % lenx, y % leny] + incx + incy) with
            | x when x < 10 -> x
            | x -> x % 10 + 1
        )

let part2 map =
    map
    |> expand
    |> shortestPath

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
