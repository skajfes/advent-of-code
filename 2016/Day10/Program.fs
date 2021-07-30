open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None

type Target =
    | Bot of int
    | Output of int
type Command =
    | Assign of int * int
    | Gives of int * Target * Target

let parse (input: string[]) =
    let toTarget target chip =
        match target with
        | "output" -> Output (int chip)
        | "bot" -> Bot (int chip)
        | _ -> failwith "unknown target"

    input
    |> Array.map (function
        | Regex "value (\d+) goes to bot (\d+)" [chip; bot] -> Assign (int chip, int bot)
        | Regex "bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)" [bot; low_output; low_chip; high_output; high_chip]
            -> Gives (int bot, toTarget low_output low_chip, toTarget high_output high_chip)
        | _ -> failwith "unknown command")

let init commands =
    commands
    |> Array.where (function | Assign _ -> true | _ -> false)
    |> Array.fold (fun bots cmd ->
        match cmd with
        | Assign (chip, bot) ->
            let chips = match Map.tryFind (Bot bot) bots with
                        | Some [chip] -> [chip]
                        | None -> []
                        | _ -> failwith "nope"

            Map.add (Bot bot) (chip::chips) bots
        | _ -> failwith "oh no") Map.empty


let doStep commands state  =
    let findChips t1 state =
        match Map.tryFind t1 state with
        | Some t -> t
        | None -> []

    let processCommands bot chips state =
        (state, commands)
        ||> Array.fold (fun state cmd ->
            match cmd with
            | Gives (b, t1, t2) when b = bot ->
                let chipsLow = findChips t1 state
                let chipsHigh = findChips t2 state
                let [low; high] = chips |> List.sort

                state
                |> Map.remove (Bot bot)
                |> Map.add t1 (low::chipsLow)
                |> Map.add t2 (high::chipsHigh)
            | _ -> state)

    (state, state)
    ||> Map.fold (fun state target chips ->
        match target, chips with
        | Bot bot, [_; _] -> processCommands bot chips state
        | _ -> state)

let part1 targetChips commands =
    let state = init commands

    let isTargetChips state =
        Map.tryPick (fun target chips ->
                     if (List.sort chips) = (List.sort targetChips)
                     then Some target
                     else None) state

    let rec repeat state =
        match isTargetChips state with
        | Some b -> Some b
        | _ ->
            let state' = doStep commands state
            if state' <> state then repeat state'
            else None

    repeat state

let part2 commands =
    let state = init commands

    let rec repeat state =
        let state' = doStep commands state
        if state' <> state then repeat state'
        else state'

    repeat state
    // take content of Output 0, 1 and 2
    |> Map.filter (fun k _ ->
        match k with
        | Output 0 | Output 1 | Output 2 -> true
        | _ -> false)
    // multiply them together
    |> Map.fold (fun res _ v -> List.head v * res) 1


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> printfn "%A"
    
    // testInput |> parse |> part1 [2; 5] |> printfn "Part 1: %A"
    input |> parse |> part1 [17; 61] |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
