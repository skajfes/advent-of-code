

let find2020 input =
    seq {for a in input do
         for b in input -> (a, b)}
    |> Seq.where (fun (a, b) -> a + b = 2020)
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.head

let find2020b input =
    seq {for a in input do
         for b in input do
         for c in input -> (a, b, c)}
    |> Seq.where (fun (a, b, c) -> a + b + c = 2020)
    |> Seq.map (fun (a, b, c) -> a * b * c)
    |> Seq.head

let testInput = [ 1721; 979; 366; 299; 675; 1456 ]

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadAllLines("input.txt")
        |> Seq.map int
        |> Seq.toList
    
    find2020 input |> printfn "Part1: %A"
    find2020b input |> printfn "Part2: %A"

    0 // return an integer exit code
