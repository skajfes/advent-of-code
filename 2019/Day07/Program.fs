open System
open System.IO

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

type Program = {
    Memory: int []
    NextOp: int
    Halted: bool
    AwaitInput: bool
}

let runCommand (program:Program) takeInput putOutput =
    let memory = program.Memory
    let pos = program.NextOp

    let op = memory.[pos]
    let argMode offset = 
        op / 100 / pown 10 (offset - 1) % pown 10 (offset)
    let read offset =
        match argMode offset with
        | 0 -> memory.[memory.[pos + offset]]
        | 1 -> memory.[pos + offset]
        | _ -> failwith "invalid arg mode"
    let write offset r =
        let resPtr = memory.[pos + offset]
        memory.[resPtr] <- r
    let command f =
        f (read 1) (read 2) |> write 3
        {program with NextOp = pos + 4} //pos + 4
    let jumpCommand f =
        if f (read 1) 
        then {program with NextOp = read 2} 
        else {program with NextOp = pos + 3}

    let input ()=
        match takeInput() with
        | Some i -> 
            write 1 i
            {program with NextOp = pos + 2}
        | None -> { program with AwaitInput = true }        

    let output () =
        let o = read 1 
        //printfn "%i" o
        putOutput o
        {program with NextOp = pos + 2}

    match op % 100 with
    | 1 -> command (+)
    | 2 -> command (*)
    | 3 -> input()
    | 4 -> output()
    | 5 -> jumpCommand ((<>) 0)
    | 6 -> jumpCommand ((=) 0)
    | 7 -> command (fun a b -> if a < b then 1 else 0)
    | 8 -> command (fun a b -> if a = b then 1 else 0)
    | 99 -> {program with Halted = true }
    | _ -> failwith (sprintf "unknown opcode: %d" op)

let run input program  =
    let mutable inputStream = 
        match input with 
        | Some x -> seq [x]
        | None -> seq []
    let takeInput() =
        match Seq.tryHead inputStream with
        | Some res -> 
            inputStream <- Seq.tail inputStream
            Some res
        | None -> None

    let mutable output = []
    let takeOutput o = output <- List.append output [o]

    let rec runToInput p =
        match runCommand p takeInput takeOutput with
        | { Halted = true } | { AwaitInput = true } as p' -> p'
        | p' -> runToInput p'

    (runToInput {program with Halted = false; AwaitInput = false }, List.tryHead output)
    
let parse (input: string) =
    input   
    |> (fun x -> x.Split(','))
    |> Array.map int


let compile text = { 
    Memory = parse text 
    NextOp = 0
    Halted = false
    AwaitInput = false
}

let testAmp programText phases =
    let init x = compile programText |> run (Some x) |> fst
    let sections = List.map init phases

    let rec amplify sections input =
        let s', o' = List.mapFold (run) input sections
        match List.exists (fun s -> s.Halted) s' with
        | false -> amplify s' o'
        | true -> o'

    match amplify sections (Some 0) with
    | Some x -> x
    | None -> failwith "no output"

    
let findPhases program range =
    range
    |> permute 
    |> List.map (testAmp program)
    |> List.max

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText ("input.txt") 

    // part 1
    // findPhases input [0..4]

    // part2
    findPhases input [5..9]
