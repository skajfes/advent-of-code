open System
open System.Collections.Generic
open System.IO
open FParsec

type Pulse = Low | High
let invert = function
    | Low -> High
    | High -> Low
let add (count_low, count_high) =
    function
    | Low -> count_low + 1, count_high
    | High -> count_low, count_high + 1

type ModuleType =
    | Broadcaster
    | Conjunction of Map<string, Pulse> // last received value for all inputs
    | FlipFlop of Pulse // last state for module
    | Output


type Modules = Map<string, (ModuleType * string[])>

let pType = choice [pstring "&"; pstring "%"; pstring "broadcaster"]
            |>> function
                | "&" -> Conjunction Map.empty
                | "%" -> FlipFlop Low
                | "broadcaster" -> Broadcaster

let pModule = pType .>>. manyChars asciiLower .>> pstring " -> " .>>. sepBy1 (many1Chars asciiLower) (pstring ", ")
              |>> fun ((moduleType, name), next) -> name, (moduleType, next)

let parse (input: string[]) =
    let modules =
        input
        |> Array.map (fun line ->
                      match run pModule line with
                      | Success (s, _, _) -> s
                      | _ -> failwith "nope")

    (Map.ofArray modules, modules)
    ||> Array.fold (fun modules (name, (modul, next)) ->
        (modules, next)
        ||> List.fold (fun modules target ->
            match modules |> Map.tryFind target with
            | Some (Conjunction sources, m_targets) ->
                if Map.containsKey name sources then modules else
                    let sources = sources |> Map.add name Low
                    modules |> Map.add target (Conjunction sources, m_targets)
            | _ -> modules))

let press_button stop modules =

    let queue = Queue<string * string * Pulse>()
    queue.Enqueue("button", "", Low)

    let rec send count modules =
        if queue.Count = 0 then count, modules else
        let sender, current_module, pulse = queue.Dequeue()

        let count = add count pulse

        match modules |> Map.tryFind current_module with
        | None -> send count modules
        | Some (modul, targets) ->

        let next_pulse, modul' =
            match modul with
            | Broadcaster -> Some pulse, Broadcaster
            | FlipFlop p ->
                match pulse with
                | High -> None, FlipFlop p
                | Low -> Some (invert p), FlipFlop (invert p)
            | Conjunction inputs ->
                let inputs' = inputs |> Map.add sender pulse
                let output =
                    if inputs' |> Map.forall (fun k v -> v = High) then Low else High
                Some output, Conjunction inputs'
            | Output -> None, Output

        // printfn "%s %A -> %s: %A -> %A %A" sender pulse current_module next_pulse targets modul'

        // update modules with changes
        let modules = modules |> Map.add current_module (modul', targets)

        match next_pulse with
        | None -> ()
        | Some next_pulse ->
            targets
            |> List.iter (fun t -> queue.Enqueue(current_module, t, next_pulse))

        match stop, (current_module, next_pulse) with
        | Some (m, p), x when (m, Some p) = x -> (-1, -1), modules
        | _ ->

        send count modules

    send (0, 0) modules



let part1 modules =
    let rec repeat count (ll, hh) modules =
        if count = 0 then (ll, hh) else
            let (l, h), modules = press_button None modules
            repeat (count - 1) (ll+l, hh+h) modules

    repeat 1000 (0, 0) modules
    |> fun (l, h) -> l * h

let rec gcd a b =
    if a = 0L then b else
    gcd (b % a) a

let rec lcm (arr: int64 list) =
    match arr with
    | [x] -> x
    | a::arr ->
        let b = lcm arr
        (a*b/gcd a b)

let part2 modules =
    let rec find stop i modules =
        let (l, h), modules = press_button (Some stop) modules
        if (l, h) = (-1, -1) then i else
        find stop (i + 1) modules

    let lk = find ("lk", High) 1 modules
    let fn = find ("fn", High) 1 modules
    let fh = find ("fh", High) 1 modules
    let hh = find ("hh", High) 1 modules

    [lk; fn; fh; hh]
    |> List.map int64
    |> lcm


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample3.txt")
    
    // testInput |> parse |> part1 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
