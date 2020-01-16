// Learn more about F# at http://fsharp.org
// #load "IntCode.fs"
// #load "Heap.fs"
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharpx.Collections

let (+|) f a b = f b a

let toAscii (input:string) = 
    input
    |> (+) +| "\n"
    |> Seq.map int64
    |> Seq.toList

let fromAscii (output:int64 list) =
    output
    |> List.map (fun c -> 
        if c < 255L then (char >> string) c
        else string c)
    |> String.concat ""


let rec runInteractive input droid:unit =
    let droid, output = IntCode.run input droid
    if droid.State = IntCode.Halted then () else 
    fromAscii output |> printfn "%s"
    let input = Console.ReadLine()
    runInteractive (toAscii (input.TrimEnd())) droid


// let text = "== Pressure-Sensitive Floor ==\nAnalyzing...\n\nDoors here lead:\n- south\n\nA loud, robotic voice says \"Alert! Droids on this ship are heavier than the detected value!\" and you are ejected back to the checkpoint.\n\n\n\n== Security Checkpoint ==\nIn the next room, a pressure-sensitive floor will verify your identity.\n\nDoors here lead:\n- north\n- east\n\nCommand?\n"

let parseOutput text =
    // printfn "%s" text
    let room = Regex.Matches(text, "== (.*) ==") |> Seq.last |> fun m -> m.Groups.[1].Value
    
    if room = "Pressure-Sensitive Floor" then printfn "%s" text

    let find pattern = 
        Regex.Matches(text, pattern)
        |> Seq.tryLast
        |> function
            | Some m -> 
                m.Groups.[1].Captures
                |> Seq.map (fun c -> c.Value)
                |> Seq.toList
            | None -> []

    let doors = find "Doors here lead:\n(?:- (\w+)\n)+"
    let items = find "Items here:\n(?:- (.+)\n)+"

    (room, doors, items)

let run input droid =
    let rawInput = toAscii input
    let droid, rawOutput = IntCode.run rawInput droid
    let text = fromAscii rawOutput
    parseOutput text, droid

let items =  [
    ("infinite loop", 0)
    ("giant electromagnet", 0)
    ("molten lava", 0)
    ("photons", 0)
    ("escape pod", 0)
    ("sand", 1)
    ("space heater", 2)
    ("loom", 4)
    ("wreath", 8)
    ("space law space brochure", 16)
    ("pointer", 32)
    ("planetoid", 64)
    ("festive hat", 128)]

let ignored = 
    List.filter (snd >> (=) 0) items
    |> List.map fst

let visitAll droid = 
    let rec visit input droid (visited: HashSet<string>) = 
        let (room, directions, items), droid'= run input droid
        if visited.Contains(room) then [] else 
        visited.Add(room) |> ignore
        directions
        |> List.collect (fun dir -> visit dir droid' visited)
        |> List.append [(room, items)]
    visit "" droid (HashSet<string>())

let pickup its droid =
    its
    |> List.filter (fun i -> List.contains i ignored |> not)
    |> List.fold (fun droid i -> 
        let cmd = sprintf "take %s" i
        let rawInput = toAscii cmd
        IntCode.run rawInput droid |> fst) droid

type VisitedState = int * string // weight * room
type State = VisitedState * IntCode.Program * string 
let weight = List.sumBy (fun i -> List.find (fst >> (=)i) items |> snd) 
 
let cartesian directions items: (string * string list) list = 
    directions
    |> List.fold (fun res dir -> 
        let res =
            items 
            |> List.fold (fun res item -> 
                (dir, [item]) :: res
            ) res
        (dir, []) :: res
    ) []
    
// cartesian ["north"; "south"] ["item"; "item 2"]

let rec visitNodeDijstra nodes (visited: HashSet<VisitedState*String>) =

    let (state, droid, command), nodes = PriorityQueue.pop nodes

    if visited.Contains(state, command) then visitNodeDijstra nodes visited else

    let (room, doors, foundItems), droid = run command droid
    
    if room = "Pressure-Sensitive Floor" then droid else
    
    // find all neighbours
    let neighbours =
        cartesian doors foundItems
        |> List.map (fun (cmd, items) -> (cmd, items, fst state + weight items))
        |> List.filter (fun (cmd, items, weight) -> visited.Contains((weight, room), command) |> not)
        |> List.map (fun (cmd, items, weight) -> (cmd, weight, pickup items droid))

    // add to nodes
    let nodes =
        neighbours
        |> List.fold (fun nodes (cmd, weight, droid) -> 
            PriorityQueue.insert ((weight, room), droid, cmd) nodes
        ) nodes

    // printfn "current: %A %s" state command
    // printfn "  neighbours: %A" (neighbours |> List.map (fun (s, i, p) -> (s, i)))
    // printfn "  nodes: %A" (nodes |> Seq.map (fun (s, p, c) -> (s, c) |> Seq.filter (fun c -> Seq.contains c (visited))))
    // Console.ReadKey() |> ignore 

    visited.Add(state, command) |> ignore
    visitNodeDijstra nodes visited 

let visitAllDijsktra droid =
    let (room, doors, items), _ = run "" droid
    let nodes =
        PriorityQueue.empty false
        |> PriorityQueue.insert ((0, room), droid, "")

    let visited = HashSet<VisitedState*string>()
    visitNodeDijstra nodes visited 


[<EntryPoint>]
let main argv =
    let droid = IO.File.ReadAllText("input.txt") |> IntCode.compile

    visitAllDijsktra droid
    |> ignore
    
    // runInteractive [] droid
    
    // visitAll droid
    // |> List.map snd
    // |> List.concat

    0 // return an integer exit code
