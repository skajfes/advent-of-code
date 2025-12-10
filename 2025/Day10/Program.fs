open System.IO
open FParsec
open System.Collections.Generic

let lightsP = between (pstring "[") (pstring "]") (many1 (pchar '.' <|> pchar '#'))
let buttonP = between (pstring "(") (pstring ")") (sepBy1 pint32 (pchar ','))
let joltageP = between (pstring "{") (pstring "}") (sepBy1 pint32 (pchar ','))
let machineP = lightsP .>> spaces .>>. many1 (buttonP .>> spaces) .>>. joltageP

//let charToInt lights = List.foldBack (fun c n -> 2*n + if c = '#' then 1 else 0) lights 0
let charToInt lights = lights |> List.map (fun c -> if c = '#' then 1 else 0) |> List.toArray
//let toInt = List.fold (fun n c -> pown 2 c + n) 0
let toInt = id

let parse (input: string[]) = 
    input
    |> Array.map (run machineP)
    |> Array.map (function 
        | Success (((lights, buttons), joltage), _, _) -> lights |> charToInt, buttons |> List.map toInt, joltage |> List.toArray
        | _ -> failwith "nope")

let shortest_path op (lights: int[]) (buttons: int list list) =

    printfn "turning on machine %A" lights

    let visited = HashSet<int[]>()

    let rec do_walk (queue: PriorityQueue<int * int[], int>) =

        if queue.Count = 0 then None else

        let presses, lights_status = queue.Dequeue()
        //printfn " cnt: %d, status: %A" presses lights_status 

        if lights_status = lights then Some presses else

        if visited.Contains lights_status then do_walk queue else
        visited.Add lights_status |> ignore

        buttons
        |> List.iter (fun p -> 
            //printfn " adding %A, %A" (op lights_status p) (p)
            queue.Enqueue((presses + 1, op lights_status p), presses + 1))

        do_walk queue

    let queue = PriorityQueue<int * int[], int>()
    queue.Enqueue((0, lights |> Array.map (fun i -> 0)), 0)

    do_walk queue


let toggle (lights: int[]) buttons = 
    lights |> Array.mapi (fun i l -> if List.contains i buttons then l^^^1 else l)
let increment (lights: int[]) buttons = 
    lights |> Array.mapi (fun i l -> if List.contains i buttons then l+1 else l)

let part1 machines =
    machines
    |> Array.map (fun (l, b, j) -> shortest_path toggle l b)
    |> Array.map (function 
        | Some l -> l
        | _ -> failwith "oh no")
    |> Array.sum

let part2 machines =
    machines
    |> Array.map (fun (l, b, j) -> shortest_path increment j b)
    |> Array.map (function 
        | Some l -> l
        | _ -> failwith "oh no")
    |> Array.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    //testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"
    
    //input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
