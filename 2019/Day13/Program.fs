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

type Tile =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball
    | Score of int64

let rec toTiles input =
    let toTile =
        function
        | 0L -> Empty
        | 1L -> Wall
        | 2L -> Block
        | 3L -> Paddle
        | 4L -> Ball
        | _ -> failwith "wrong tile"
    match input with
    | [] -> []
    | -1L::0L::score::rest -> ((-1L, 0L), Score score) :: toTiles rest
    | x::y::z::rest -> ((x,y), toTile z) :: toTiles rest
    | _ -> failwith "wrong input length"

let findPos tile tiles =
    tiles 
    |> List.filter (snd >> (=)tile) 
    |> List.head 
    |> fst

let play game =

    let rec runFrame input game = 
        let game, output = run input game
        let tiles = toTiles output

        match game with
        | { Halted = true } -> 
            tiles
            |> List.filter (function | _, Score _ -> true | _ -> false)
            |> List.head
            |> snd
        | _ -> 
            let ballPosition = findPos Ball tiles
            let paddlePosition = findPos Paddle tiles

            let direction =
                if fst ballPosition < fst paddlePosition then -1L
                else if fst ballPosition > fst paddlePosition then 1L
                else 0L

            runFrame direction game
    
    runFrame 0L game

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText ("input.txt") 

    let game = input |> compile
    //let _, output = run 0L game

    // part 1
    // toTiles output
    // |> List.filter (snd >> (=)Block)
    // |> List.length

    // part 2
    // add coins
    let game = input |> compile
    game.Memory.[0] <- 2L
    play game



    0
    
