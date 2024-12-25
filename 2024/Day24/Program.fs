open System.IO

let parse (input: string[]) = 
    input
    |> Array.fold (fun (wires, rules) line ->
        if line.Contains(':') then
            let [| wire; value |] = line.Split(": ")
            ((wire, if value = "1" then true else false )::wires, rules)
        elif line.Contains("->") then
            let [| rule; wire |] = line.Split(" -> ")
            let [| w1; r; w2 |] = rule.Split(" ")
            (wires, (r, w1, w2, wire)::rules)
        else
            (wires, rules)
        ) ([], [])

let read_input wires =
    wires
    |> List.map

let compile rules =
    let rec comp output =
        match rules |> List.filter (fun (_, _, _, o) -> o = output) with
        | [] -> fun wires -> wires |> List.find (fun (w, v) -> w = output) |> snd
        | [(r, a, b, o)] ->
            let fa = comp a
            let fb = comp b
            match r with
            | "AND" -> fun wires -> (fa wires) && (fb wires)
            | "OR" -> fun wires -> (fa wires) || (fb wires)
            | "XOR" -> fun wires -> (fa wires) <> (fb wires)
            | _ -> failwith "Unknown rule"
        | _ -> failwith "Multiple rules"

    let z_wires =
        rules
        |> List.map (fun (_, _, _, o) -> o)
        |> List.filter (fun (o:string) -> o.StartsWith("z"))
        |> List.sortDescending

    z_wires
    |> List.map comp


let execute wires rules =
    rules
    |> List.map (fun f -> f wires)
    |> List.fold (fun res w -> 2L*res + (if w then 1L else 0L)) 0L

let part1 (wires, rules) =
    compile rules // compile the rules to a function that calculates the value of a wire
    |> execute wires

let rec to_binary bits x =
    if bits = 0 then [] else
        (x%2L = 1L) :: to_binary (bits - 1) (x/2L)

let to_wires bits x y =
    []
    |> List.append (to_binary bits x |> List.mapi (fun i v -> sprintf "x%02d" i, v))
    |> List.append (to_binary bits y |> List.mapi (fun i v -> sprintf "y%02d" i, v))

let part2 (wires, rules) =
    let f = compile rules
    [0..44]
    |> List.map (fun i -> i, 1L<<<i)
    |> List.map (fun (i, j) -> i, j, execute (to_wires 45 0L j) f)
    |> List.filter (fun (i, j, v) -> j <> v)
    |> List.map (fun (i, j, v) -> i)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // input |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %A"
    // part2 prints which bits are off,
    // render adders in mermaid graph and analyze the circuit to see which wires are swapped

    0 // return an integer exit code
