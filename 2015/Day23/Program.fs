

type Register = A | B
type Command =
    | Half of Register
    | Triple of Register
    | Increment of Register
    | Jump of int
    | JumpIfEven of Register * int
    | JumpIfOdd of Register * int

let reg (input: string) =
    match input with
    | "a" -> A
    | "b" -> B
    | _ -> failwith "unknown register"
    
let offset (input: string) =
    int32 input
    
let trim (s: string) = s.Trim()
let regAndOffset (input: string) =
    let [| r; o |] = input.Split(',') |> Array.map trim
    (reg r, offset o)
    
let (|Prefix|_|) (prefix: string) (input: string) =
    if input.StartsWith(prefix)
    then Some (input.Substring(4))
    else None
    
let command (input: string) =
    match input with
    | Prefix "hlf" args -> Half (reg args)
    | Prefix "tpl" args -> Triple (reg args)
    | Prefix "inc" args -> Increment (reg args)
    | Prefix "jmp" args -> Jump (offset args)
    | Prefix "jie" args -> JumpIfEven (regAndOffset args)
    | Prefix "jio" args -> JumpIfOdd (regAndOffset args)
    | _ -> failwithf "unknown command: %s" input
    
let parse (input:string) =
    input.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map trim
    |> Array.map command
    
    
let runProgram (startA, startB) program =
    let rec runCommand (current, a, b) =
        let register cmd r =
            match r with
            | A -> (current + 1, cmd a, b)
            | B -> (current + 1, a, cmd b)
        
        let jump f r o =
            let v = match r with | A -> a | B -> b
            let o = if f v then o else 1
            (current + o, a, b)
            
        match Array.tryItem current program with
        | None -> (a, b)
        | Some c ->
            let state = 
                match c with 
                | Half r -> register (fun a -> a / 2) r
                | Triple r -> register ((*)3) r
                | Increment r -> register ((+)1) r
                | Jump o -> (current + o, a, b)
                | JumpIfEven (r, o) -> jump (fun v -> v % 2 = 0) r o
                | JumpIfOdd (r, o) -> jump (fun v -> v = 1) r o
            
            runCommand state
        
    runCommand (0, startA, startB) |> snd
    
let testInput = "inc a
jio a, +2
tpl a
inc a"

[<EntryPoint>]
let main argv =
    
    let program = System.IO.File.ReadAllText("input.txt") |> parse
    
    runProgram (0, 0) program
    |> printfn "Part1: %A"
    
    runProgram (1, 0) program
    |> printfn "Part2: %A"
    
    0 // return an integer exit code