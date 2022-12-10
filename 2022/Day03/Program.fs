open System.IO

type Backpack = char[]

let toBackpack input : Backpack * Backpack =
    let [| a; b |] = input |> Seq.toArray |> Array.splitInto 2
    (a, b)

let parse (input: string[]) = input |> Array.map toBackpack

let points e =
    if System.Char.IsUpper(e) then
        e - 'A' |> int |> (+) 27
    else
        e - 'a' |> int |> (+) 1

let part1 (backpacks: (Backpack * Backpack)[]) =
    backpacks
    |> Array.map (fun (a, b) -> a |> Array.find (fun e -> Array.contains e b))
    |> Array.sumBy points

let findSameElement [| a; b; c |] =
    a
    |> Array.filter (fun e -> b |> Array.contains e)
    |> Array.filter (fun e -> c |> Array.contains e)
    |> Array.head

let part2 backpacks =
    backpacks
    |> Array.map (fun (a, b) -> Array.append a b |> Array.distinct)
    |> Array.chunkBySize 3
    |> Array.map findSameElement
    |> Array.sumBy points

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
