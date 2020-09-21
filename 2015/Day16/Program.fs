open System
open System.Text.RegularExpressions

type Sue = {
    No: int
    Items: (string * int) list
}

let parseLine line =
    // Sue 1: cars: 9, akitas: 3, goldfish: 0
    let m = Regex.Match(line, "Sue (\d+): (?:(\w+): (\d+)(?:, )?)+")
    let no = m.Groups.[1].Value |> int
    let items = 
        [0..Seq.length m.Groups.[2].Captures - 1]
        |> List.map (fun i ->
            let thing = m.Groups.[2].Captures.[i].Value
            let count = m.Groups.[3].Captures.[i].Value |> int
            (thing, count))
        
    { No = no; Items = items }
    
let parse input =
    Seq.map parseLine input
    |> Seq.toList
    
let filter filterItem filterCount f sues =
    sues
    |> List.filter (fun sue ->
                    match List.tryFind (fun (item, _) -> item = filterItem) sue.Items with
                    | Some (_, count) when f count filterCount -> true
                    | Some (_, _) -> false
                    | None -> true)

[<EntryPoint>]
let main argv =
    let sues = IO.File.ReadAllLines("input.txt") |> parse
    
    sues
    |> filter "children" 3 (=)
    |> filter "cats" 7 (=)
    |> filter "samoyeds" 2 (=)
    |> filter "pomeranians" 3 (=)
    |> filter "akitas" 0 (=)
    |> filter "vizslas" 0 (=)
    |> filter "goldfish" 3 (=)
    |> filter "trees" 3 (=)
    |> filter "cars" 2 (=)
    |> filter "perfumes" 1 (=)
    |> printfn "part 1: %A"
    
    sues
    |> filter "children" 3 (=)
    |> filter "cats" 7 (>)
    |> filter "samoyeds" 2 (=)
    |> filter "pomeranians" 3 (<)
    |> filter "akitas" 0 (=)
    |> filter "vizslas" 0 (=)
    |> filter "goldfish" 3 (<)
    |> filter "trees" 3 (>)
    |> filter "cars" 2 (=)
    |> filter "perfumes" 1 (=)
    |> printfn "part 2: %A"
    
    0 // return an integer exit code
