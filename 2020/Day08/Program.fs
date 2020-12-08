open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
let testInput =
    """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6""".Split("\r\n") |> Seq.toList

type Command =
    | Accumulate of int
    | Jump of int
    | Noop of int

type State =
    | Running of (int * int)
    | Looping of int
    | Done of int
    
let (|Prefix|_|) prefix input =
    let m = Regex.Match(input, sprintf @"^%s ((?:\+|-)\d+)$" prefix)
    if m.Success then Some (m.Groups.[1].Value |> int)
    else None
    
let parse input =
    let command cmd =
        match cmd with
        | Prefix "acc" arg -> Accumulate arg
        | Prefix "jmp" arg -> Jump arg
        | Prefix "nop" arg -> Noop arg
        | _ -> failwith "unknown command"
    List.map command input
    |> List.toArray

let runToEnd program =
    let visited = HashSet<int>()
    
    let rec run (accumulator, step) =
        let runCommand =
            function
            | Noop _ -> Running (accumulator, step + 1)
            | Accumulate x -> Running (accumulator + x, step + 1)
            | Jump x -> Running (accumulator, step + x)
            
        if visited.Contains(step)
        then Looping accumulator
        else
            visited.Add(step) |> ignore
            match Array.tryItem step program with
            | None -> Done accumulator
            | Some cmd ->
                match runCommand cmd with
                | Running state -> run state
                | state -> state
        
    run (0, 0) 

let generateFixedPrograms program =
    let replaceCmd cmdAt =
        Array.mapi (fun i cmd ->
            match i = cmdAt, cmd with
            | true, Jump x -> Noop x
            | true, Noop x -> Jump x
            | _ -> cmd ) program
        
    seq {
        let toReplace =
            program
            |> Array.indexed
            |> Array.filter (fun (_, cmd) ->
                match cmd with
                | Noop x | Jump x -> true
                | _ -> false)
        for i in toReplace do
            yield (replaceCmd (fst i))
    }
    
let fixProgram program =
    program
    |> generateFixedPrograms 
    |> Seq.map runToEnd
    |> Seq.filter (function
        | Done x -> true
        | _ -> false)
    |> Seq.head
        
    
[<EntryPoint>]
let main argv =
    let program = File.ReadAllLines("input.txt") |> Seq.toList |> parse
    program |> runToEnd |> printfn "Part 1: %A"
    program |> fixProgram |> printfn "Part 2: %A"
    
    0 // return an integer exit code