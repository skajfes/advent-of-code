open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open FSharpx.Collections

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some
        <| List.tail
            [ for g in m.Groups do
                  [ for c in g.Captures -> c.Value ] ]
    else
        None

let parse (input: string[]) =
    input
    |> Array.map (function
        | Regex "Valve (..) .*=(\d+); .* valves? (?:(..)(?:, )?)+" [ [ valve ]; [ rate ]; tunnels ] -> valve, int rate, tunnels
        | s -> failwithf "oops %s" s)


type State =
    { Time: int
      CurrentRoom: string
      UnopenedValves: Map<string, int> }

// let findBiggestFlow valves =
//
//     let valves = valves
//                  |> Array.map (fun (v, f, rooms) -> v, (v, f, rooms))
//                  |> Map.ofArray
//
//     let visited = HashSet<State>()
//     let rec find (queue: IPriorityQueue<int * State>) =
//         let (total_flow, state), queue = queue |> PriorityQueue.pop
//
//
//         if state.Time < 0 then
//             find queue
//         elif visited.Contains(state) then
//             find queue
//         elif state.Time = 0 then
//             total_flow
//         else
//             printfn "%d %d %s %A" (30 - state.Time) total_flow state.CurrentRoom state.UnopenedValves
//             visited.Add(state) |> ignore
//             let queue =
//                 match Map.tryFind state.CurrentRoom state.UnopenedValves with
//                 | Some rate ->
//                     let flow = (state.Time - 1) * rate
//                     let state = { state with Time = state.Time - 1; UnopenedValves = state.UnopenedValves |> Map.remove state.CurrentRoom }
//                     queue |> PriorityQueue.insert (total_flow + flow, state)
//                 | None ->
//                     queue
//
//             let rooms = valves |> Map.find state.CurrentRoom |> fun (_, _, rooms) -> rooms
//             printfn "next: %A" rooms
//
//             (queue, rooms)
//             ||> List.fold (fun queue room ->
//                 let state = { state with CurrentRoom = room; Time = state.Time - 1 }
//                 queue |> PriorityQueue.insert (total_flow, state)
//                 )
//             |> find
//
//     let state =
//         { Time = 30
//           CurrentRoom = "AA"
//           UnopenedValves = valves
//                            |> Map.map (fun _ (_, rate, _) -> rate)
//                            |> Map.filter (fun _ rate -> rate > 0)
//                        }
//
//     let queue = PriorityQueue.empty true |> PriorityQueue.insert (0, state)
//     find queue

let findBiggestFlow valves =
    valves |> printfn "%A"
    let start = "AA"
    let time = 30
    let unopenedValves  = valves
                          |> Array.filter (fun (_, flow, _) -> flow > 0)
                          |> Array.map (fun (valve, _, _) -> valve)

    let rec search valve time unopenedValves =
        
        0

    search start time unopenedValves

let part1 valves = findBiggestFlow valves

[<EntryPoint>]
let main argv =
    // let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part1 |> printfn "%A"

    // input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
