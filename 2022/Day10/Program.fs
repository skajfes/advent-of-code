open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then
        Some <| List.tail [ for g in m.Groups -> g.Value ]
    else
        None

type Command =
    | Add of int
    | Noop

let parse (input: string[]) =
    input
    |> Array.map (fun c ->
        match c with
        | Regex "addx (-?\d+)" [ x ] -> Add(int x)
        | Regex "noop" [] -> Noop
        | _ -> failwith "no")

let execute cmd (reg :: rest) =
    match cmd with
    | Add x -> reg + x :: reg :: reg :: rest
    | Noop -> reg :: reg :: rest

let runProgram program =
    ([ 1 ], program) ||> Array.fold (fun reg cmd -> execute cmd reg) |> List.rev

let part1 commands =
    runProgram commands
    |> List.indexed
    |> List.map (fun (i, v) -> (i + 1, v))
    |> List.where (fun (i, v) -> (i - 20) % 40 = 0)
    // |> List.iter (fun x -> printfn "%A" x)
    |> List.sumBy (fun (x, y) -> x * y)

let part2 commands =
    runProgram commands
    |> List.indexed
    |> List.map (fun (c, x) -> if x-1<=c%40 && c%40 <= x+1 then "#" else " ")
    |> List.chunkBySize 40
    |> List.take 6
    |> List.iter (fun x -> printfn "%s" (System.String.Join("", x)))

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample2.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
