open System
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map(fun line ->
        let [| card; numbers |] = line.Split(':')
        let ns = numbers.Split('|')
                 |> Array.map (fun c -> c.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int )

        let cardNr = card.Substring(card.IndexOf(' ')) |> int
        (cardNr, ns)
    )

let part1 cards =
    cards
    |> Array.map snd
    |> Array.map (fun [| winners; numbers |] ->
        numbers
        |> Array.filter (fun n -> Array.contains n winners)
        )
    |> Array.filter (fun x -> Array.isEmpty x |> not)
    |> Array.map (fun x -> pown 2 (Array.length x - 1))
    |> Array.sum

let part2 cards =
    let copies = Array.fold (fun copies (c, _) -> Map.add c 1 copies) Map.empty cards

    (copies, cards)
    ||> Array.fold (fun copies (c, [| winners; numbers |]) ->
        let win_cards =
            numbers
            |> Array.filter (fun n -> Array.contains n winners)
            |> Array.length

        let have_copies = Map.find c copies

        (copies, [1..win_cards])
        ||> List.fold (fun new_copies cc ->
                let had = Map.find (c + cc) copies

                new_copies
                |> Map.add (c + cc) (had + have_copies)
            )
        )
    |> Map.toList
    |> List.sumBy snd


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
