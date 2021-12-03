open System.IO

let toDecimal bytes =
    bytes
    |> Seq.rev
    |> Seq.fold (fun (dec, multiplier) value -> (value |> string |> int) * multiplier + dec, multiplier * 2) (0, 1)
    |> fst

let part1 input =
    let getFactor f =
        input
        |> Seq.transpose
        |> Seq.map (Seq.countBy id)
        |> Seq.map (f snd)
        |> Seq.map (fst >> string)
        |> toDecimal

    let epsilon = getFactor Seq.maxBy
    let gamma = getFactor Seq.minBy

    epsilon * gamma

let part2 (input: seq<string>) =
    let mostCommon f position input =
        input
        |> Seq.transpose
        |> Seq.skip position
        |> Seq.head
        |> Seq.countBy id
        |> f (fun (c, cnt) -> cnt, c)
        |> fst

    let findFactor f input =
        (input, seq { 0 .. 20 })
        ||> Seq.fold (fun input position ->
                if Seq.length input = 1 then input else

                let c = mostCommon f position input
                Seq.where (fun x -> x |> Seq.skip position |> Seq.head = c) input
            )
        |> Seq.head
        |> toDecimal

    let o2rating = findFactor Seq.maxBy input
    let co2rating = findFactor Seq.minBy input

    o2rating * co2rating

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    // testInput |> part2 |> printfn "%A"
    
    input |> part1 |> printfn "Part 1: %A"
    input |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
