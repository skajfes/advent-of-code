open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    input
    |> Array.mapi(fun r row -> row |> Seq.mapi (fun c e -> (r, c), (string>>int) e))
    |> Seq.concat
    |> Seq.toArray

let ns (r, c) = [ (r-1,c); (r+1, c); (r, c-1); (r, c+1) ]

let find map (end_state:int*int -> 'a) acc_f p =
    let cache = Dictionary<int*int, 'a >()

    let rec do_find (pos, h) =
        if h = 9 then end_state pos else

        if cache.ContainsKey(pos) then cache[pos] else

        map
        |> Array.filter (fun (p, h') -> List.contains p (ns pos) && h' = h + 1)
        |> Array.map (fun (p, h') ->
            do_find (p, h')
        )
        |> acc_f
        |> fun res ->
            cache.Add(pos, res) |> ignore
            res

    do_find p

let part1 map =
    map
    |> Array.filter ((snd)>>(=)0)
    |> Array.map (find map (fun pos -> [|pos|]) (Array.concat >> Array.distinct) >> Array.length)
    |> Array.sum

let part2 map =
    map
    |> Array.filter ((snd)>>(=)0)
    |> Array.map (find map (fun _ -> 1) Array.sum)
    |> Array.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
