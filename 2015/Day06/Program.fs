// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions

type Coord = int * int

type Command =
    | TurnOn of Coord * Coord
    | TurnOff of Coord * Coord
    | Toggle of Coord * Coord

let getRange input: Coord * Coord =
    let m = Regex.Match(input, "(\d+),(\d+) through (\d+),(\d+)")
    if not m.Success then
        failwith "unable to get ranges"
    else
        let f (id: int) = m.Groups.[id].Value |> int
        (f 1, f 2), (f 3, f 4)

let parseCommand (input: string) =
    let range = getRange input
    match input with
    | x when x.StartsWith("turn on") -> TurnOn range
    | x when x.StartsWith("turn off") -> TurnOff range
    | x when x.StartsWith("toggle") -> Toggle range
    | _ -> sprintf "invalid command %s" input |> failwith

let applyCommand lights cmd (x1, y1) (x2, y2) =
    Array2D.mapi (fun x y state ->
        if x >= x1 && x <= x2 && y >= y1 && y <= y2
        then cmd state
        else state) lights
        
let doCommand lights command =
    match command with
    | TurnOn (a, b) -> applyCommand lights (fun _ -> true) a b
    | TurnOff (a, b) -> applyCommand lights (fun _ -> false) a b
    | Toggle (a, b) -> applyCommand lights (not) a b
        
let doFixedCommand lights command =
    match command with
    | TurnOn (a, b) -> applyCommand lights ((+)1) a b
    | TurnOff (a, b) -> applyCommand lights (fun x -> if x > 0 then x - 1 else 0) a b
    | Toggle (a, b) -> applyCommand lights ((+)2) a b

let lightsCount amountOfLights lights =
    let mutable count = 0
    Array2D.iter (fun c -> count <- count + (amountOfLights c)) lights
    count

[<EntryPoint>]
let main argv =
    
    let input = IO.File.ReadAllLines("input.txt")
    input
    |> Seq.map parseCommand
    |> Seq.fold doCommand (Array2D.init 1000 1000 (fun x y -> false))
    |> lightsCount (function | false -> 0 | true -> 1)
    |> printfn "part1: %d lights are on" 

    input
    |> Seq.map parseCommand
    |> Seq.fold doFixedCommand (Array2D.init 1000 1000 (fun x y -> 0))
    |> lightsCount id
    |> printfn "part2: %d lights are on"
    
    0 // return an integer exit code
