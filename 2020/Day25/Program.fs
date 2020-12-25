
let parse (input: string[]) =
    let [| pk1; pk2 |] = Array.map int64 input
    (pk1, pk2)

let transform loopSize pk =
    [1..loopSize]
    |> List.fold (fun key _ -> key * pk % 20201227L) 1L

let findLoopSize subject pk =
    let rec loop i start =
        let n = start * subject % 20201227L
        if n = pk then i else loop (i + 1) n

    loop 1 1L

let findKey doorPK cardPK =
    let loopSize = findLoopSize 7L doorPK
    transform loopSize cardPK

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")

    // Test
    findKey 5764801L 17807724L |> printfn "%d"
    // Part 1
    input |> parse ||> findKey |> printfn "Part 1: %d"
    // Part 2 :)

    0 // return an integer exit code