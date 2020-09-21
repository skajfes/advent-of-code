open System

let rec combinations sum list =
    match list with
    | [] -> [] // no elements
    | _ when sum <= 0 -> [] // we have exhausted the sum
    | h::tail when h > sum ->
        combinations sum tail // skip the current element
    | h::tail when h = sum ->
        [[h]] // current element 
        |> List.append (combinations sum tail) // and skip current element
    | h::tail ->
        combinations (sum - h) tail
        |> List.map (fun l -> h::l)
        |> List.append (combinations sum tail)

let minNumbers sum jars =
    let c = combinations sum jars |> List.map List.length
    let minLength = c |> List.min
    c |> List.filter ((=)minLength) |> List.length
    
    
[<EntryPoint>]
let main argv =
    let jars = IO.File.ReadAllLines("input.txt") |> Seq.map (int) |> Seq.toList
    combinations 150 jars |> List.length |> printfn "part1: %A"
    minNumbers 150 jars |> printfn "part2: %A"
    
    0 // return an integer exit code
    
