open System.IO

let parse (input: string[]) =
    input
    |> Seq.mapi (fun r row -> row |> Seq.mapi (fun c s -> (r, c, (string >> int) s)))
    |> Seq.concat
    |> Seq.toArray

let print squid =
    let sArray = Array2D.zeroCreate 10 10

    squid
    |> Array.iter (fun (r, c, v) -> sArray[r,c] <- v)

    printfn "%A" sArray

let neighbours r c = [|(r-1, c-1); (r-1, c); (r-1, c+1)
                       (r, c-1);            (r, c+1)
                       (r+1, c-1); (r+1, c); (r+1, c+1)|]

let third (a,b,c) = c

let blink squid =

    let rec doBlink blinked squid =
        let toBlink =
            squid
            |> Array.where (third >> (<)9)
            |> Array.map (fun (r,c,v) -> r,c)
            |> Array.except blinked

        let blinked = Array.concat [| blinked; toBlink |]

        let toIncrement =
            toBlink
            |> Array.map (fun (r,c) -> neighbours r c)
            |> Array.concat
            |> Array.filter (fun x -> Array.contains x blinked |> not)
            |> Array.countBy id

        let squid =
            squid
            |> Array.map (function
                          | r, c, v when Array.contains (r, c) toBlink -> r, c, 0
                          | r, c, v when Array.exists (fun ((rr,cc), vv)-> r = rr && c = cc) toIncrement ->
                              let vv = toIncrement |> Array.filter (fun ((rr, cc), vv) -> r = rr && c = cc) |> Array.head |> snd
                              r, c, v + vv
                          | v -> v)

        if Array.isEmpty toBlink
        then squid, Array.length blinked
        else doBlink blinked squid

    doBlink [||] squid


let step squid =
    let squid =
        squid
        |> Array.map (fun (r, c, s) -> (r, c, s+1))
    let squid, count = blink squid

    squid, count


let part1 squid =
    ((squid, 0), [1..100])
    ||> List.fold (fun (squid, count) x ->
        let squid, cnt = step squid
        squid, cnt + count)
    |> snd

let part2 squid =
    ((squid, None), [1..500])
    ||> List.fold (fun (squid, allFlashed) x ->
        match allFlashed with
        | Some x -> squid, Some x
        | None ->
            let squid, cnt = step squid
            if cnt = 100 then squid, Some x else squid, None
        )
    |> snd

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
