open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some <| List.tail [for g in m.Groups -> g.Value]
    else None

type Command =
    | Mask of string
    | Assign of int * string
    
let toBin value =
    Seq.unfold (function
        | 0 -> None
        | v -> Some (v%2, v/2)) value
    |> Seq.rev
    |> Seq.map string
    |> System.String.Concat
    |> fun s -> s.PadLeft(36, '0')
    
let toDec value =
    Seq.foldBack (fun c (n, b) ->
        n + (string c |> int64) * b, b * 2L) value (0L, 1L)
    |> fst
let command (text:string) =
    match text with
    | Regex @"^mask = ([01X]+)$" [mask] -> Mask mask
    | Regex @"^mem\[(\d+)\] = (\d+)$" [address; value] -> Assign (int address, int value |> toBin)
    | c -> failwithf "unknown command '%s'" c

let applyMaskToValue mask (addr, value) =
    let v = 
        Seq.map2 (fun m v ->
            match m with
            | 'X' -> v
            | '1' -> '1'
            | '0' -> '0'
            | _ -> failwith "mask fail") mask value
        
    [(addr, toDec v)]
    
let applyMaskToAddress mask (address, value) =
    let rec floating addr =
        match addr with
        | [] -> [[]]
        | 'X'::addrs ->
            let rest = floating addrs
            List.map (fun l -> '1'::l) rest
            |> List.append (List.map (fun l -> '0'::l) rest)
        | v::adds ->
            let rest = floating adds
            List.map (fun l -> v::l) rest
            
    Seq.map2 (fun m v ->
        match m with
        | 'X' -> 'X'
        | '1' -> '1'
        | '0' -> v
        | _ -> failwith "mask fail") mask (toBin address)
    |> Seq.toList
    |> floating 
    |> Seq.map (fun l -> (toDec l, toDec value))
    
let sumOfMemory transform input =
    input
    |> Array.map command
    |> Array.fold (fun (mask, memory) ->
        function
        | Mask mask -> mask, memory
        | Assign (addr, value) ->
            mask, transform mask (addr, value)
                  |> Seq.fold (fun memory (a, v) ->
                      Map.add a v memory) memory
            ) ("", Map.empty)
    |> snd
    |> Map.fold (fun sum _ v -> sum + v) 0L
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    input |> sumOfMemory applyMaskToValue |> printfn "Part 1: %d"
    input |> sumOfMemory applyMaskToAddress |> printfn "Part 2: %d"
    
    0 // return an integer exit code