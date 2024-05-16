open System.Collections.Generic
open System.Diagnostics
open System.IO
open FParsec

// run ALU with parsing the commands
type Variable = W|X|Y|Z
type Argument =
    | Variable of Variable
    | Value of int
type Command =
    | Inp of Variable
    | Add of Variable * Argument
    | Mul of Variable * Argument
    | Div of Variable * Argument
    | Mod of Variable * Argument
    | Eql of Variable * Argument

let p_var = choice [pchar 'w'; pchar 'x'; pchar 'y'; pchar 'z']
            |>> function
                | 'w' -> W
                | 'x' -> X
                | 'y' -> Y
                | 'z' -> Z
                | _ -> failwith "no"
let p_val = (pint32 |>> Value) <|> (p_var |>> Variable)
let p_arg1 = spaces1 >>. p_var
let p_arg2 = spaces1 >>. p_var .>> spaces1 .>>. p_val

let p_inp = pstring "inp" >>. p_arg1 |>> Inp
let p_cmd c f = pstring c >>. p_arg2 |>> f
let p_add = p_cmd "add" Add
let p_mul = p_cmd "mul" Mul
let p_div = p_cmd "div" Div
let p_mod = p_cmd "mod" Mod
let p_eql = p_cmd "eql" Eql

let p_command = choice [ p_inp; p_add; p_mul; p_div; p_mod; p_eql ]

let parse (input: string[]) = 
    input
    |> Array.map (fun x -> run p_command x |> function | Success (x, _, _) -> x | Failure (e, _, _) -> failwith e)

let setMemory (w, x, y, z) var value =
    match var with
    | W -> (value, x, y, z)
    | X -> (w, value, y, z)
    | Y -> (w, x, value, z)
    | Z -> (w, x, y, value)

let getMemory (w, x, y, z) var =
    match var with
    | W -> w
    | X -> x
    | Y -> y
    | Z -> z

let runCommand input mem cmd =
    let value a =
        match a with
        | Value x -> x
        | Variable x -> getMemory mem x

    let twoArgsOp f a b =
        input, setMemory mem a (f (getMemory mem a) (value b))

    match cmd with
    | Inp x -> Array.tail input, setMemory mem x (Array.head input)
    | Mul(a, b) -> twoArgsOp (*) a b
    | Add(a, b) -> twoArgsOp (+) a b
    | Div(a, b) -> twoArgsOp (fun a b -> a / b) a b
    | Mod(a, b) -> twoArgsOp (fun a b -> a % b) a b
    | Eql(a, b) -> twoArgsOp (fun a b -> if a = b then 1 else 0) a b


let runProgram input program =
    ((input, (0, 0, 0, 0)), program)
    ||> Array.fold (fun (input, mem) cmd -> runCommand input mem cmd)
    |> fun (_, (w, x, y, z)) -> z

// manually analyse input and code up the program
let step input divz addx addy z =
    let t = (truncate (float z / float divz) |> int)
    if (z % 26) + addx <> input
    then 26 * t + input + addy
    else t

let run_program2 (input: int[]) =
    // printfn "\n"
    0
    |> step input[0] 1 13 8
    |> step input[1] 1 12 13
    |> step input[2] 1 12 8
    |> step input[3] 1 10 10
    |> step input[4] 26 -11 12
    |> step input[5] 26 -13 1
    |> step input[6] 1 15 13
    |> step input[7] 1 10 5
    |> step input[8] 26 -2 10
    |> step input[9] 26 -6 3
    |> step input[10] 1 14 2
    |> step input[11] 26 0 2
    |> step input[12] 26 -15 12
    |> step input[13] 26 -4 7


// function created analysing the solution and behavior
// only possible 0 results if some sn numbers are generated based on previous numbers

let number_generator (s: int[]) =
    Array.init 14 (fun i ->
        match i with
        | 0 -> s[0]
        | 1 -> s[1]
        | 2 -> s[2]
        | 3 -> s[3]
        | 4 -> s[3] - 1
        | 5 -> s[2] - 5
        | 6 -> s[4]
        | 7 -> s[5]
        | 8 -> s[5] + 3
        | 9 -> s[4] + 7
        | 10 -> s[6]
        | 11 -> s[6] + 2
        | 12 -> s[1] - 2
        | 13 -> s[0] + 4
        | _ -> failwith "todo"
        )

let serial_numbers asc generator =
    if asc then
        [1111111..9999999]
    else
        [9999999..-1..1111111]
    |> Seq.map (string >> Seq.map (string >> int) >> Seq.toArray)
    |> Seq.map generator
    |> Seq.filter (fun s -> s |> Array.forall (fun i -> i >= 1 && i <= 9))

let solve asc program =
    serial_numbers asc number_generator
    |> Seq.map (fun i -> System.String.Join("", i), run_program2 i )
    |> Seq.filter (snd >> (=) 0)
    |> Seq.map fst
    |> Seq.head

let part1 program =
    solve false program

let part2 program =
    solve true program


[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    input |> parse |> part1 |> printfn "Part 1: %s"
    input |> parse |> part2 |> printfn "Part 2: %s"

    0 // return an integer exit code
