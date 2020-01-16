module IntCode 

open System
open System.IO

type Program = {
    Memory: Map<int, int64>
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
            if offset < memory.Count
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
            if offset < memory.Count 
            then 
                let m = Map.add offset v memory
                { program with Memory = m }
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
    let mutable input = input
    let readInput() = 
        //printfn "request input. %A" input
        //Console.ReadLine() |> int64 |> Some
        // printfn "reading input %A" input
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

let compile (text: string) = 
    let parse =
        text
        |> (fun x -> x.Split(','))
        |> Array.map int64
        |> Array.indexed
        |> Map.ofArray
    
    { 
        Memory = parse 
        VirtualMemory = []
        NextOp = 0
        RelativeBase = 0
        Halted = false
        AwaitInput = false
    }