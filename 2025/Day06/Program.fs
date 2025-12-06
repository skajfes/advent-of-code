open System
open System.IO

let part1 (input: string[]) =
    let (ops::args) = input
                        |> Array.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
                        |> Array.toList
                        |> List.rev
    let ops = ops |> List.map (function | "*" -> (*) | "+" -> (+) | _ -> failwith "nope")
    let args = args |> List.map (List.map int64) |> List.transpose

    List.zip ops args

let part2 (input: string[]) =
    let ops = input[^0]
              |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
              |> Array.map (function | "*" -> (*) | "+" -> (+) | _ -> failwith "nope")
              |> Array.toList

    let args =
        input[0..^1]
        |> Seq.transpose
        |> Seq.rev
        |> Seq.map (fun x -> Seq.filter ((<>)' ') x)
        |> Seq.map (fun x -> Seq.map (string>>int64) x)
        |> Seq.map (fun x -> Seq.fold (fun n c -> 10L*n + c) 0L x)
        |> Seq.fold (fun (a::b) c -> match c with | 0L -> []::a::b | _ -> (c::a)::b) [[]]
        |> Seq.toList

    List.zip ops args

let sum ops =
    ops
    |> List.map (fun (op, args) -> List.reduce op args)
    |> List.sum


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> part1 |> sum |> printfn "%A"
    testInput |> part2 |> sum |> printfn "%A"

    input |> part1 |> sum |> printfn "Part 1: %d"
    input |> part2 |> sum |> printfn "Part 2: %d"

    0 // return an integer exit code
