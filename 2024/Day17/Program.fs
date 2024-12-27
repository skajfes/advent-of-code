open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then
        Some <| List.tail
            [ for g in m.Groups -> g.Value ]
    else None

let parse (input: string[]) = 
    input
    |> Array.fold (fun (registers, program) line ->
        match line with
        | Regex "Register ([ABC]): (\d+)" [reg; value] -> (Map.add reg (int64 value) registers, program)
        | Regex "Program: (.*)" [p] -> (registers,
                                        p.Split(',')
                                        |> Array.map int64
                                        )
        | _ -> registers, program
        ) (Map.empty, [||])

let run_program (registers, program) =
    let rec run instruction_pointer output (registers: Map<string, int64>) =
        let reg r = registers |> Map.find r
        let store r v = Map.add r v registers
        let operand() = program |> Array.item (instruction_pointer + 1)
        let combo() =
            match operand() with
            | 0L | 1L | 2L | 3L as op -> op
            | 4L -> reg "A"
            | 5L -> reg "B"
            | 6L -> reg "C"
            | _ -> failwith "invalid combo operator"

        match Array.tryItem instruction_pointer program with
        | None -> output
        | Some op ->
            match op with
            | 0L ->
                // adv - division
                reg "A" / (pown 2L (combo() |> int))
                |> store "A"
                |> run (instruction_pointer + 2) output
            | 6L ->
                // bdv
                reg "A" / (pown 2L (combo() |> int))
                |> store "B"
                |> run (instruction_pointer + 2) output
            | 7L ->
                // cdv
                reg "A" / (pown 2L (combo() |> int))
                |> store "C"
                |> run (instruction_pointer + 2) output
            | 1L ->
                // bxl - bitwise xor
                reg "B" ^^^ operand()
                |> store "B"
                |> run (instruction_pointer + 2) output
            | 2L ->
                // bst - modulo 8
                (combo()) % 8L
                |> store "B"
                |> run (instruction_pointer + 2) output
            | 3L ->
                // jnz - jump if not zero
                if reg "A" = 0 then
                    run (instruction_pointer + 2) output registers
                else
                    run (operand() |> int) output registers
            | 4L ->
                // bxc - bitwise xor
                reg "B" ^^^ reg "C"
                |> store "B"
                |> run (instruction_pointer + 2) output
            | 5L ->
                // out - output
                let output = (combo() % 8L)::output
                run (instruction_pointer + 2) output registers
            | _ -> failwith "invalid opcode"

    run 0 [] registers
    |> List.rev
    |> Array.ofList

let find_quine (registers, program) =
    // program processes register A in chunks of 3 bits (1 octal digit) from right to left
    // first output numbers depend on the next so process in reverse order,
    // find digit by digit (in octal) starting from msb
    let rec find i n =
        if i = Array.length program then [n] else
        [0..7]
        |> List.map (fun x -> n*8L + int64 x)
        |> List.filter (fun n ->
            let registers = Map.add "A" n registers
            run_program (registers, program)
            |> fun output -> output = program[(program.Length - i - 1)..] // match last (n-i) digits
        )
        |> List.collect (fun n -> find (i+1) n)

    find 0 0L
    |> List.min

let part1 (registers, program) =
    run_program (registers, program)
    |> Array.map string
    |> String.concat ","

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> run_program |> printfn "%A"
    // testInput |> parse |> find_quine |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %s"
    input |> parse |> find_quine |> printfn "Part 2: %d"

    0 // return an integer exit code
