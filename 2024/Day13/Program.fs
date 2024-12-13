open System.IO
open FParsec

let pButton b = pstring "Button " >>. pchar b
let pCoord = choice [ pchar 'X'; pchar 'Y' ] >>. pchar '+' >>. pfloat |>> fun a -> decimal a
let pButtonRow b = pButton b >>. pstring ": " >>. pCoord .>> pstring ", " .>>. pCoord .>> pchar '\n'
let pCoord2 c = pchar c >>. pchar '=' >>. pfloat |>> fun a -> decimal a
let pPrize = pstring "Prize: " >>. pCoord2 'X' .>> pstring ", " .>>. pCoord2 'Y'

let pMachine = pButtonRow 'A' .>>. pButtonRow 'B' .>>. pPrize .>> pchar '\n' |>> fun ((a, b), p) -> (a, b, p)
let pMachines = sepBy1 pMachine (pchar '\n')

let parse (input: string) =
    match run pMachines input with
    | Success (res, s, r) -> res
    | Failure (s, e, r) -> failwith s

let play ((xa, ya), (xb, yb), (xp, yp)) =
    let nb = (yp -  ya*xp/xa)/(yb - xb*ya/xa)
    let na = (xp - nb * xb) / xa
    if
       abs (round na - na) < 0.0000001m &&
       abs (round nb - nb) < 0.0000001m
    then Some (round na, round nb)
    else None

let part1 machines =
    machines
    |> List.map play
    |> List.filter (function | Some (na, nb) -> na > 0m && na < 100m && nb > 0m && nb <= 100m | _ -> false) // check bounds
    |> List.sumBy (function | Some (a, b) -> 3m*a+b | None -> 0m)

let part2 machines =
    machines
    |> List.map (fun (a, b, (px, py)) -> a, b, (10000000000000m + px, 10000000000000m + py))
    |> List.map play
    |> List.sumBy (function | Some (a, b) -> 3m*a+b | None -> 0m)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let testInput = File.ReadAllText("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
