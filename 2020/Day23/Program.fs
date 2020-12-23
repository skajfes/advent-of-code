open System.Diagnostics
#nowarn "57"

let parse (input: string) =
    input |> Seq.toArray |> Array.map (string >> int)

let moreCups cups =
    Array.append cups [|Array.length cups + 1..1_000_000|]

let serialize (linked_list: int[]) =
    [1..linked_list.Length - 2]
    |> List.fold (fun (res, idx) _ ->
        let e = linked_list.[idx]
        res + string e, e) ("", 1)
    |> fst

let serialize2 (list: int[]) =
    let a = list.[1]
    let b = list.[a]
    int64 a * int64 b

let arrayGame totalRounds cups =
    // initialize linked array
    let linked = Array.create (Array.length cups + 1) -1
    Array.pairwise cups
    |> Array.iter (fun (cup, next) -> linked.[cup] <- next)
    // add link for last element
    linked.[cups.[^0]] <- cups.[0]

    let rec find_destination dest m1 m2 m3 =
        if dest < 1 then find_destination (linked.Length - 1) m1 m2 m3 else
        if dest = m1 || dest = m2 || dest = m3
        then find_destination (dest - 1) m1 m2 m3
        else dest

    let rec playRound round current =
        if round > totalRounds then linked else

        // three elements that move
        let m1 = linked.[current]
        let m2 = linked.[m1]
        let m3 = linked.[m2]
        // the rest of the list
        let rest = linked.[m3]

        // remove element from list
        linked.[current] <- rest

        // find destination
        let dest = find_destination (current - 1) m1 m2 m3

        // insert elements after dest
        linked.[m3] <- linked.[dest]
        linked.[dest] <- m1

        playRound (round + 1) (linked.[current])

    playRound 1 cups.[0]


[<EntryPoint>]
let main argv =
    let input = "167248359"
    let testInput = "389125467"

    let sw = Stopwatch.StartNew()

    input |> parse |> arrayGame 100 |> serialize |> printfn "Part 1: %s"

    let p1 = sw.ElapsedMilliseconds
    sw.Restart()

    input |> parse |> moreCups |> arrayGame 10_000_000 |> serialize2 |> printfn "Part 2: %d"

    let p2 = sw.ElapsedMilliseconds

    printfn "Part1: %d ms, Part2 %d ms" p1 p2

    0 // return an integer exit code