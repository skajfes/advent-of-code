module IntCode 

type State =
    | Halted
    | AwaitInput
    | HaveOutput
    | Running

type Program = {
    Memory: Map<int, int64>
    NextOp: int
    RelativeBase: int
    State: State
}

let compile (text: string) = {
    Memory = text
        |> (fun x -> x.Split(','))
        |> Array.map int64
        |> Array.indexed
        |> Map.ofArray 
    NextOp = 0
    RelativeBase = 0
    State = Running
}

let private runCommand (program:Program) takeInput putOutput =
    let memory = program.Memory
    let pos = program.NextOp

    let op = memory.[pos] |> int
    let argMode offset = 
        op / 100 / pown 10 (offset - 1) % 10
    
    let read offset =
        let safeRead offset =
            match Map.tryFind offset memory with
            | Some v -> v
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
            let m = Map.add offset v memory
            { program with Memory = m }
        
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
        | None -> { program with State = AwaitInput }        

    let output () =
        let state = read 1 |> putOutput
        { program with NextOp = pos + 2; State = state }

    let adjustRelativeBase() =
        let offset = read 1 |> int
        { program with RelativeBase = program.RelativeBase + offset; NextOp = pos + 2 }

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
    | 99 -> {program with State = Halted }
    | _ -> failwith (sprintf "unknown opcode: %d" (op % 100))

let run input program =
    let mutable input = input
    let readInput() = 
        match input with
        | r::tail -> 
            input <- tail
            Some r 
        | _ -> None

    let mutable output = []
    let writeOutput o = 
        output <- output @ [o]
        if List.length output = 3 then HaveOutput else Running
        
    let rec runToEnd p =
        match runCommand p readInput writeOutput with
        | { State = Running } as p -> runToEnd p
        | p -> p

    match program with
    | { State = Halted } -> (program, output)
    | _ ->
        let program' = runToEnd { program with State = Running }
        (program', output)

