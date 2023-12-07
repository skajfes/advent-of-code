open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (_.Split(' ') >> fun [| hand; bid |] -> hand, bid |> int)

let map hand jokers =
    hand
    |> Seq.toArray
    |> Array.map (
        function
        | 'J' when jokers -> 0x0
        | '2' -> 0x1
        | '3' -> 0x2
        | '4' -> 0x3
        | '5' -> 0x4
        | '6' -> 0x5
        | '7' -> 0x6
        | '8' -> 0x7
        | '9' -> 0x8
        | 'T' -> 0x9
        | 'J' -> 0xa
        | 'Q' -> 0xb
        | 'K' -> 0xc
        | 'A' -> 0xd
        | _ -> failwith "nope")

let rank hand =
    fst hand
    |> Seq.groupBy id
    |> Seq.map (fun (card, cs) -> Seq.length cs)
    |> Seq.toList
    |> List.sortDescending
    |> function
        | [5] -> 7 // five of kind
        | [4; 1] -> 6 // four of kind
        | [3; 2] ->  5 // full house
        | [3; 1; 1] -> 4 // three of kind
        | [2; 2; 1] -> 3 // two pair
        | [2; 1; 1; 1] -> 2 // pair
        | [1; 1; 1; 1; 1] -> 1 // high card
        | a -> failwithf "nope %A" a
    |> fun x -> x, map (fst hand) false, snd hand

let rank' hand =
    fst hand
    |> Seq.filter ((<>)'J') // remove all jokers
    |> Seq.groupBy id
    |> Seq.map (fun (card, cs) -> Seq.length cs)
    |> Seq.toList
    |> List.sortDescending
    |> function
        // no jokers
        | [5] -> 7 // five of kind
        | [4; 1] -> 6 // four of kind
        | [3; 2] ->  5 // full house
        | [3; 1; 1] -> 4 // three of kind
        | [2; 2; 1] -> 3 // two pair
        | [2; 1; 1; 1] -> 2 // pair
        | [1; 1; 1; 1; 1] -> 1 // high card
        // one joker
        | [4] -> 7 // five of kind
        | [3; 1] -> 6 // four of kind
        | [2; 2] ->  5 // full house
        | [2; 1; 1] -> 4 // three of kind
        | [1; 1; 1; 1] -> 2 // pair
        // two jokers
        | [3] -> 7 // five of kind
        | [2; 1] -> 6 // four of kind
        | [1; 1; 1] -> 4 // three of kind
        // three jokers
        | [2] -> 7 // five of kind
        | [1; 1] -> 6 // four of kind
        // four jokers
        | [1] -> 7 // five of kind
        // five jokers
        | [] -> 7 // five of kind
        | a -> failwithf "nope %A" a
    |> fun x -> x, map (fst hand) true, snd hand


let game rank hands =
    hands
    |> Array.map rank
    |> Array.sort
    |> Array.mapi (fun i (_, _, hand) -> (i + 1) * hand)
    |> Array.sum

let part1 hands = game rank hands
let part2 hands = game rank' hands

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
