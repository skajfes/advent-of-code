open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    let map =
        input
        |> Array.mapi (fun r row ->
            row |> Seq.mapi (fun c v -> (r, c), v) |> Seq.toArray)
        |> Array.concat
        |> Array.filter (snd >> (<>)'#')
    let start =
        map |> Array.find (snd >> (=)'S') |> fst

    map |> Array.map fst, start, input |> Array.length


let traverse map start =

    let queue = Queue<(int*int)*int>()
    let visited = Dictionary<int*int, int>()

    let neighbours (r, c) map =
        [|
            (r-1, c)
            (r+1, c)
            (r, c-1)
            (r, c+1)
        |]
        |> Array.filter (fun x -> Array.contains x map)

    let rec walk () =
        if queue.Count = 0 then visited else

        let pos, step = queue.Dequeue()

        if visited.ContainsKey(pos) then walk() else
        visited.Add(pos, step)

        neighbours pos map
        |> Array.iter (fun pos -> queue.Enqueue(pos, step+1))

        walk()

    queue.Enqueue(start, 0)
    walk ()

let part1 steps map start len =
    let visited = traverse map start
    let odd = if steps % 2 = 0 then 0 else 1
    visited.Values |> Seq.filter (fun d -> d % 2 = odd && d <= steps) |> Seq.length

let part2 steps map start len =
    let dmax = len / 2

    let visited = traverse map start
    let even_corners = visited.Values |> Seq.filter (fun d -> d % 2 = 0 && d > dmax) |> Seq.length |> int64
    let odd_corners = visited.Values |> Seq.filter (fun d -> d % 2 = 1 && d > dmax) |> Seq.length |> int64
    let even_full = visited.Values |> Seq.filter (fun d -> d % 2 = 0) |> Seq.length |> int64
    let odd_full = visited.Values |> Seq.filter (fun d -> d % 2 = 1) |> Seq.length |> int64

    let n = (steps - dmax) / len |> int64

    let p = ((n+1L)*(n+1L) * odd_full) + (n*n*even_full) - ((n+1L)*odd_corners) + (n*even_corners)
    p


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    input |> parse |||> part1 64 |> printfn "Part 1: %d"
    input |> parse |||> part2 26501365 |> printfn "Part 2: %d"

    0 // return an integer exit code
