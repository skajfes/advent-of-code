open System.IO
open System.Text.RegularExpressions

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then Some(s.Substring(p.Length))
    else None

let parse_line (line: string) =
    Regex.Matches(line, "(mul\((\d{1,3}),(\d{1,3})\)|(do(?:n't)?)\(\))")
    |> Seq.map (fun x -> x.Groups[1].Value, x.Groups[2].Value, x.Groups[3].Value )
    |> Seq.toArray

let parse (input: string[]) =
    input
    |> Array.collect parse_line

let part1 matches =
    matches
    |> Array.filter (fun (op:string, _, _) -> op.StartsWith("mul")) // ignore do's and don'ts
    |> Array.sumBy (fun (_, a, b) -> (int a) * (int b))

let part2 matches =
    matches
    |> Array.fold (fun (sum, enabled) (op, a, b) ->
                  match op with
                  | Prefix "don't" _ -> sum, false
                  | Prefix "do" _ -> sum, true
                  | Prefix "mul" _ when enabled -> (int a * int b) + sum, true
                  | Prefix "mul" _ -> sum, false
                  ) (0, true)
    |> fst

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    let testInput2 = File.ReadAllLines("sample2.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput2 |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
