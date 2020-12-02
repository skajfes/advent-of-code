let testInput = [
    "1-3 a: abcde";
    "1-3 b: cdefg";
    "2-9 c: ccccccccc";
]

let validNumberOfChars (c, min, max, pass) =
    pass
    |> Seq.filter ((=)c)
    |> Seq.length
    |> (fun x -> x >= min && x <= max)

let validCharAtPosition (c, min, max, pass) =
    let a = Seq.item (min - 1) pass
    let b = Seq.item (max - 1) pass
    (a = c || b = c) && a <> b
    
let parse (input: string) =
    let m = System.Text.RegularExpressions.Regex.Match(input, @"(?<min>\d+)-(?<max>\d+) (?<char>\w): (?<pass>\w+)")
    let pass = m.Groups.["pass"].Value
    let min = m.Groups.["min"].Value |> int
    let max = m.Groups.["max"].Value |> int
    let char = m.Groups.["char"].Value.[0]
    (char, min, max, pass)
   
let validate validator input =
    input
    |> Seq.map parse
    |> Seq.filter validator
    |> Seq.length
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    
    validate validNumberOfChars input |> printfn "Part 1: %d"
    validate validCharAtPosition input |> printfn "Part 2: %d"
    
    0 // return an integer exit code