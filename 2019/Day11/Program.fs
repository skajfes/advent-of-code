open System
open System.IO

type Program = {
    Memory: int64 []
    VirtualMemory: (int * int64) list
    NextOp: int
    RelativeBase: int
    Halted: bool
    AwaitInput: bool
}

let runCommand (program:Program) takeInput putOutput =
    let memory = program.Memory
    let pos = program.NextOp

    let op = memory.[pos] |> int
    let argMode offset = 
        op / 100 / pown 10 (offset - 1) % 10
    
    let read offset =
        let safeRead offset =
            if offset < memory.Length 
            then memory.[offset]
            else match List.tryFind (fst >> (=)offset) program.VirtualMemory with
                 | Some (_, v) -> v
                 | None -> 0L            

        match argMode offset with
        | 0 -> 
            let ptr = safeRead (pos + offset) |> int
            safeRead ptr
        | 1 -> 
            safeRead (pos + offset)
        | 2 -> 
            let relativeOffset = safeRead (pos + offset) |> int
            safeRead (program.RelativeBase + relativeOffset)
        | _ -> failwith "invalid arg mode"

    let write offset r =
        let safeWrite offset v =
            if offset < memory.Length 
            then 
                memory.[offset] <- v
                program
            else
                let removed = List.filter (fst >> (<>)offset) program.VirtualMemory 
                { program with VirtualMemory = (offset, v)::removed }
        
        let resPtr = memory.[pos + offset] |> int
        match argMode offset with
        | 0 -> safeWrite resPtr r
        | 1 -> failwith "invalid argMode for writing"
        | 2 -> safeWrite (program.RelativeBase + resPtr) r
        | _ -> failwith "invalid argMode w"

    let command f =
        let p' = f (read 1) (read 2) |> write 3
        { p' with NextOp = pos + 4 } 
    let jumpCommand f =
        if f (read 1) 
        then { program with NextOp = read 2 |> int }  
        else { program with NextOp = pos + 3 }

    let input ()=
        match takeInput() with
        | Some i -> 
            let p' = write 1 i
            { p' with NextOp = pos + 2 }
        | None -> { program with AwaitInput = true }        

    let output () =
        let o = read 1 
        putOutput o
        { program with NextOp = pos + 2 }

    let adjustRelativeBase() =
        let offset = read 1 |> int
        { program with RelativeBase = program.RelativeBase + offset; NextOp = pos + 2 }

    // printfn "op: %d, pos: %d" op pos
    // printfn "state: %A" program
    // Console.ReadKey() |> ignore

    match op % 100 with
    | 1 -> command (+)
    | 2 -> command (*)
    | 3 -> input()
    | 4 -> output()
    | 5 -> jumpCommand ((<>) 0L)
    | 6 -> jumpCommand ((=) 0L)
    | 7 -> command (fun a b -> if a < b then 1L else 0L)
    | 8 -> command (fun a b -> if a = b then 1L else 0L)
    | 9 -> adjustRelativeBase()
    | 99 -> {program with Halted = true }
    | _ -> failwith (sprintf "unknown opcode: %d" (op % 100))

let run input program =
    let mutable input = [input]
    let readInput() = 
        //printfn "request input. %A" input
        //Console.ReadLine() |> int64 |> Some
        match List.tryHead input with
        | Some r -> 
            input <- List.tail input
            Some r 
        | None -> None

    let mutable output = []
    let writeOutput o = 
        //printfn "output: %d" o
        output <- output @ [o]

    let rec runToEnd p =
        match runCommand p readInput writeOutput with
        | { Halted = true } | { AwaitInput = true } as p' -> p'
        | p' -> runToEnd p'

    let program' = runToEnd {program with Halted = false; AwaitInput = false }
    (program', output)

let parse (input: string) =
    input   
    |> (fun x -> x.Split(','))
    |> Array.map int64


let compile text = { 
    Memory = parse text 
    VirtualMemory = []
    NextOp = 0
    RelativeBase = 0
    Halted = false
    AwaitInput = false
}

type Direction =
    | Up
    | Down
    | Left
    | Right

type Robot = {
    Position: int*int
    Direction: Direction
    Memory: Map<(int*int),int64>
}

let makeRobot = {
    Position = (0,0)
    Direction = Up
    Memory = Map.empty.Add((0,0), 1L)
}

let getColor robot =
    match Map.tryFind robot.Position robot.Memory with
    | Some color -> color
    | None -> 0L

let paint color robot =
    let m = Map.add robot.Position color robot.Memory
    { robot with Memory = m }

let turn dir robot =
    let d =
        match dir with
        | 0L -> 
            match robot.Direction with
            | Up -> Left
            | Down -> Right
            | Left -> Down
            | Right -> Up
        | 1L -> 
            match robot.Direction with
            | Up -> Right
            | Down -> Left
            | Left -> Up
            | Right -> Down
        | t -> sprintf "invalid turn %d" t |> failwith
    { robot with Direction = d }

let move robot =
    let dx, dy =
        match robot.Direction with
        | Up -> (0, 1)
        | Down -> (0, -1)
        | Left -> (-1, 0)
        | Right -> (1, 0)
    let x, y = robot.Position
    { robot with Position = (x+dx, y+dy) }

let rec runRobot robot brain =
    let (brain, output) = run (getColor robot) brain
    if List.length output < 2 
    then (robot, brain)
    else 
    let robot =
        robot
        |> paint output.[0] 
        |> turn output.[1]
        |> move
    
    runRobot robot brain

let draw memory =
    let memoryList = Map.toList memory
    let minX = List.map (fst >> fst >> int) memoryList |> List.min 
    let maxX = List.map (fst >> fst >> int) memoryList |> List.max
    let minY = List.map (fst >> snd >> int) memoryList |> List.min 
    let maxY = List.map (fst >> snd >> int) memoryList |> List.max

    let whites = 
        memoryList
        |> List.map (fun ((x, y), c) -> ((y - minY, x - minX), c)) 
        |> List.filter (snd >> (=) 1L)
        |> List.map fst

    [for _ in [0..maxY - minY] -> [0..maxX - minX]]
    |> List.mapi 
            (fun row cols ->  
                List.map (fun c -> 
                        match List.contains (row, c) whites with 
                        | true -> "#" 
                        | false -> " ") cols 
                |> String.concat "")            
    |> List.rev
    |> List.iter (printfn "%s")


[<EntryPoint>]
let main argv =
    let input = File.ReadAllText ("input.txt") 

    let brain  = compile input
    let (r, _) = runRobot makeRobot brain
    let memory = r.Memory
    // part 1
    // Map.count r.Memory
    draw r.Memory

    0
    
