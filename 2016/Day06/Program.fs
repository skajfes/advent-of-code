
let parse (input: string[]) =
    input

let analyzeFrequency desc (data: string[]) =
    data
    |> Seq.transpose
    |> Seq.map (Seq.groupBy id
                >> Seq.map (fun (c, cs) -> c, Seq.length cs)
                >> (if desc then Seq.sortByDescending else Seq.sortBy) snd
                >> Seq.head
                >> fst)
    |> System.String.Concat

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")

    input |> parse |> analyzeFrequency true |> printfn "Part 1: %s"
    input |> parse |> analyzeFrequency false |> printfn "Part 2: %s"

    0 // return an integer exit code