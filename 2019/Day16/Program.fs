open System

let basePattern = [0; 1; 0; -1]

let patternList p = seq {
    let s = Seq.initInfinite (fun i -> basePattern.[i/p%4])
    yield! Seq.skip 1 s
}

let pattern p e =
    basePattern.[((e+1) / (p+1) % 4)]

let position p input =
    let l = List.length input
    List.zip input (Seq.take l (patternList p) |> Seq.toList) 
    |> List.sumBy (fun (a, b) -> a * b)
    |> (fun m -> abs m % 10)

let positions p input =
    let l = List.length input
    List.zip input (Seq.take l (patternList p) |> Seq.toList) 
    |> List.sumBy (fun (a, b) -> a * b)
    |> (fun m -> abs m % 10)


let rec phase n input =
    let l = List.length input
    let s = [for p in 1..l -> position p input]
    if n = 1 
    then s
    else phase (n-1) s

let rec phase2 input n =
    let result = 
        Seq.foldBack (fun el digits -> 
            let last = 
                match List.tryHead digits with
                | Some d -> d
                | None -> 0
            abs (last + el) % 10 :: digits) (input) []

    if n = 1 
    then result
    else phase2 result (n-1)



let parse (input: string) = Seq.map (string>>int) input |> Seq.toList

let toInt input =
    List.foldBack (fun e (m, r) -> (m * 10, r + e*m)) input (1, 0) |> snd

[<EntryPoint>]
let main argv =
    let input = parse "02935109699940807407585447034323" 
    let input = IO.File.ReadAllText("input.txt") |> parse
    // phase 100 input |> List.take 8 |> toInt

    // let i = List.replicate 100 input |> List.concat
    // 100 * positions 6 i
    // position 6 i

    // part 2

    let offset = List.take 7 input |> toInt
    let i = input |> List.replicate 10000 |> List.concat |> List.skip offset
   
    phase2 i 100 |> List.take 8 |> toInt
