open System
open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (_.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries))
    |> Array.fold (fun (a, b) [| ia; ib |] -> (int ia)::a, (int ib)::b ) ([], [])

let part1 (a, b) =
    List.zip (List.sort a) (List.sort b)
    |> List.map (fun (a, b) -> abs(b - a))
    |> List.sum

let part2 (a, b) =
    let grouped = List.groupBy id b
                  |> List.map (fun (a, b) -> a, List.length b)
                  |> Map.ofList

    a
    |> List.sumBy (fun x -> match grouped.TryFind(x) with
                            | Some g -> g * x
                            | None -> 0)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
