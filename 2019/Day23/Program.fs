// #load "IntCode.fs"
open System

let rec run computers inputs natMsg =
    let computers, inputs =
        computers
        |> List.mapFold (fun inputs (id, comp) -> 
            let input, inputs =
                match Map.find id inputs with
                | x::y::ins -> 
                    let inputs = Map.add id ins inputs
                    [x; y], inputs
                | x::ins -> 
                    let inputs = Map.add id ins inputs
                    [x], inputs
                | [] -> [-1L], inputs

            let comp, output = IntCode.run input comp

            let inputs = 
                match output with
                | [255L; x; y] -> 
                    printfn "set nat msg %d" y 
                    //exit 0
                    Map.add 255 [x; y] inputs
                | [addr; x; y] -> 
                    printfn "message: %A" (addr, x, y)
                    let input =
                        inputs
                        |> Map.find (int addr)
                        |> List.append [x; y]
                    Map.add (int addr) input inputs
                | [] -> inputs
                | _ -> failwith "invalid output"

            (id, comp), inputs
        ) inputs

    let allIdle = computers |> List.forall (fun (id, c) -> c.State = IntCode.AwaitInput && Map.find id inputs |> (=) []) 
    if allIdle && Map.containsKey 255 inputs then     
        let m = Map.find 255 inputs
        if Some m = natMsg then
            printfn "done: %A" m
            exit 0
        else printfn "nat msg: %A" m
        let inputs = Map.add 0 m inputs 
        run computers inputs (Some m)
    else run computers inputs natMsg

[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllText("input.txt") 
    
    let computers =
        IntCode.compile input 
        |> List.replicate 50
        |> List.indexed

    let inputs =
        computers
        |> List.map (fun (i, _) -> i, [int64 i])
        |> Map.ofList
    
    run computers inputs None
    |> ignore

    0 // return an integer exit code
