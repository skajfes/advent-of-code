// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions

type Signal =
    | Wire of string
    | Value of uint16
    
type Output =
    | OutputWire of string

type Command =
    | Is of Signal * Output
    | And of Signal * Signal * Output
    | Or of Signal * Signal * Output
    | LShift of Signal * int * Output
    | RShift of Signal * int * Output
    | Not of Signal * Output
    
let output cmd =
    match cmd with
    | Is (_, w) -> w
    | And (_, _, w) -> w
    | Or (_, _, w) -> w
    | LShift (_, _, w) -> w
    | RShift (_, _, w) -> w
    | Not (_, w) -> w

let parseSignal (s: string) =
    match UInt16.TryParse(s) with
    | true, v -> Value v
    | _ -> Wire s
    
let (|OneSignal|_|) cmd input =
    let space = if cmd = "" then "" else " "
    let m = Regex.Match(input, sprintf "^%s%s(.+)$" cmd space)
    if (m.Success)
    then m.Groups.[1].Value |> parseSignal |> Some
    else None
    
let (|TwoSignals|_|) cmd input =
    let m = Regex.Match(input, sprintf "^(.+) %s (.+)$" cmd)
    if (m.Success)
    then Some (m.Groups.[1].Value |> parseSignal, m.Groups.[2].Value |> parseSignal)
    else None
    
let (|SignalAndConst|_|) cmd input =
    let m = Regex.Match(input, sprintf "^(.+) %s (\d+)$" cmd)
    if (m.Success)
    then Some (m.Groups.[1].Value |> parseSignal, m.Groups.[2].Value |> int)
    else None
    
let parse input =
    let m = Regex.Match(input, "(.+) -> (\w+)")
    if m.Success then
        let o = OutputWire m.Groups.[2].Value
        match m.Groups.[1].Value with
        | TwoSignals "AND" (a, b) -> And (a, b, o)
        | TwoSignals "OR" (a, b) -> Or (a, b, o)
        | SignalAndConst "RSHIFT" (a, c) -> RShift (a, c, o)
        | SignalAndConst "LSHIFT" (a, c) -> LShift (a, c, o)
        | OneSignal "NOT" a -> Not (a, o)
        | OneSignal "" i -> Is (i, o)
        | _ -> failwith "unknown command"
    else
        failwith "invalid command (missing ->)"
    
let getInputs cmd =
    match cmd with
    | Is (a, _) -> [a]
    | Not (a, _) -> [a]
    | And (a, b, _) -> [a; b]
    | Or (a, b, _) -> [a; b]
    | RShift (a, _, _) -> [a]
    | LShift (a, _, _) -> [a]
    
    
let result wire config =

    let mutable cache = Map.empty

    let rec recResult wire =
        match Map.tryFind wire cache with
        | Some v -> v
        | None ->
        
        let ow = OutputWire wire
        let cmd = Seq.where (output >> (=)ow) config |> Seq.head
    
        let value =
            function
            | Wire a -> recResult a 
            | Value a -> a
            
        let v = 
            match cmd with
            | Is (a, _) -> value a
            | Not (a, _) -> ~~~ (value a)
            | And (a, b, _) -> (value a) &&& (value b)
            | Or (a, b, _) -> (value a) ||| (value b)
            | RShift (a, s, _) -> (value a) >>> s
            | LShift (a, s, _) -> (value a) <<< s
        cache <- Map.add wire v cache
        
        v
    
    recResult wire

[<EntryPoint>]
let main argv =
    
    let input = IO.File.ReadAllLines("input.txt")
    let config = Seq.map parse input
    let a = result "a" config
    printfn "part1: %d" a
    
    let config' =
        config
        |> Seq.filter (fun c -> output c <> OutputWire "b") 
        |> Seq.append (seq [Is (Value a, OutputWire "b")])
    let a' = result "a" config'
    printfn "part2: %d" a'
    
    0 // return an integer exit code
