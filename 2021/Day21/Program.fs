open System.Collections.Generic
open System.IO

let parse (input: string[]) = 
    let p1 = input[0].Split(':')[1] |> int
    let p2 = input[1].Split(':')[1] |> int
    p1, p2

let rec play cube p1 p2 =

    let roll = ((cube + 0) % 100 + 1) + ((cube + 1)%100 + 1) + ((cube + 2) % 100 + 1)
    let f = (fst p1 + roll) % 10
    let p1 = (f, snd p1 + f + 1)

    if snd p1 >= 1000 then snd p2 * (cube + 3) else
        play (cube + 3) p2 p1

let playDirac p1 p2 =
    let visited = Dictionary<(int*int)*(int*int), (int64*int64)>()

    let rec recPlay p1 p2 =

        if visited.ContainsKey(p1, p2) then visited[(p1, p2)] else

        [for r1 in 1..3 do for r2 in 1..3 do for r3 in 1..3 -> r1+r2+r3]
        |> List.fold (fun (s1, s2) roll ->
            let f = (fst p1 + roll) % 10
            let p1 = (f, snd p1 + f + 1)

            if snd p1 >= 21 then (s1 + 1L, s2) else
                let (ps2, ps1) = recPlay p2 p1
                (s1 + ps1, s2 + ps2)
            ) (0L, 0L)
        |> fun s ->
            visited[(p1, p2)] <- s
            s

    recPlay p1 p2

let part1 (p1, p2) =
    play 0 (p1-1, 0) (p2-1, 0)

let part2 (p1, p2) =
    playDirac (p1-1, 0) (p2-1, 0)
    |> fun (s1, s2) ->
        if s1 > s2 then s1 else s2

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
