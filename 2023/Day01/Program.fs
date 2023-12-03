open System
open System.IO

let parse (input: string[]) = input

let toCalibrationNumber (line: string) =
    let firstNumber =
        line[line.IndexOfAny("1234567890" |> Seq.toArray)] |> string |> int

    let lastNumber =
        line[line.LastIndexOfAny("1234567890" |> Seq.toArray)] |> string |> int

    10 * firstNumber + lastNumber



let firstNumber f g (line: string) =
    [ ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9)
      ("1", 1)
      ("2", 2)
      ("3", 3)
      ("4", 4)
      ("5", 5)
      ("6", 6)
      ("7", 7)
      ("8", 8)
      ("9", 9)
      ("0", 0) ]
    |> Seq.map (fun x -> g line (fst x), snd x)
    |> Seq.where (fun x -> fst x >= 0)
    |> f fst
    |> (fun x -> snd x)

let toCalibrationNumber2 (line: string) =
    let a = firstNumber (Seq.minBy) (fun (l: string) (x: string) -> l.IndexOf(x, StringComparison.Ordinal)) line
    let b = firstNumber (Seq.maxBy) (fun (l: string) (x: string) -> l.LastIndexOf(x, StringComparison.Ordinal)) line
    10 * a + b

let part1 = Array.sumBy toCalibrationNumber
let part2 = Array.sumBy toCalibrationNumber2

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample2.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
