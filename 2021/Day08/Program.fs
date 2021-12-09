open System
open System.IO

let toSegs (input:string) =
    input.Split(' ')
    |> Array.map (fun s -> s.Trim())
    |> Array.where (fun s -> s <> "")
let parse (input: string[]) =
    input
    |> Array.map (fun (line: string) -> line.Split('|')
                                          |> function
                                              | [| sample; output |] -> (toSegs sample, toSegs output))

let part1 inputs =
    inputs
    |> Array.map (fun (_, output) ->
                  output
                  |> Array.map (fun s ->
                      match String.length s with
                      | 2 | 3 | 4 | 7 -> 1
                      | _ -> 0)
                  |> Array.sum)
    |> Array.sum


let digits = [
    (0, "abcefg")
    (1, "cf")
    (2, "acdeg")
    (3, "acdfg")
    (4, "bcdf")
    (5, "abdfg")
    (6, "abdefg")
    (7, "acf")
    (8, "abcdefg")
    (9, "abcdfg")
]

let analyze (input, output) =
    let byLength n = input |> Array.where (fun s -> String.length s = n)
    let one = byLength 2 |> Array.head
    let seven = byLength 3 |> Array.head
    let four = byLength 4 |> Array.head
    let only i = i |> Array.where (Seq.length >> (=)1) |> Seq.head |> Seq.head
    let without c num = num |> Seq.filter (fun s -> s <> c)
    let a = seven |> Seq.filter (fun c -> one.Contains(c) |> not) |> Seq.head
    let g = byLength 6 |> Array.map (fun s -> s |> Seq.filter ((<>)a) |> Seq.filter (fun s -> four.Contains(s) |> not)) |> only
    let d = byLength 5 |> Array.map (fun s -> s |> Seq.filter ((<>)a) |> Seq.filter ((<>)g) |> Seq.filter (fun s -> one.Contains(s) |> not)) |> only
    let b = four |> Seq.filter (fun s -> (one + (string d)).Contains(s) |> not) |> Seq.head
    let f = byLength 5 |> Array.map (fun s -> s |> Seq.filter ((<>)a) |> Seq.filter ((<>)b) |> Seq.filter ((<>)d) |> Seq.filter ((<>)g)) |> only
    let c = one |> without f |> Seq.head
    let e = byLength 5
            |> Array.map (fun s -> s
                                   |> Seq.filter ((<>)a)
                                   |> Seq.filter ((<>)c)
                                   |> Seq.filter ((<>)d)
                                   |> Seq.filter ((<>)g)
                                   |> Seq.filter ((<>)b)
                                   |> Seq.filter ((<>)f)) |> only

    let map = [
        ('a', a)
        ('b', b)
        ('c', c)
        ('d', d)
        ('e', e)
        ('f', f)
        ('g', g)
    ]
    output, map

let transformDigit map dig =
    let transformed =
        dig
        |> Seq.map (fun c -> map |> List.filter (fun (a, b) -> b = c) |> List.map fst |> List.head)
        |> Seq.sort
        |> Seq.toList
        // |> String.concat ""

    digits
    |> List.filter (fun (d, segs) -> Seq.toList segs = transformed)
    |> List.map fst
    |> List.head

let transform (output, map) =
    output
    |> Array.map (transformDigit map)
    |> Array.map string
    |> String.concat ""
    |> int

let part2 inputs =
    inputs
    |> Array.map (analyze >> transform)
    |> Array.sum


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    // let testInput = [|"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"|]
    
    // testInput |> parse |> part1 |>  printfn "%A"
    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
