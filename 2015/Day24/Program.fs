

open System.ComponentModel.DataAnnotations
let findIdealConfiguration parts presents =
    
    let rec combinations goal presents =
        match goal with
        | 0 -> [[]]
        | g when g < 0 -> [[g]]
        | _ -> 
            List.where ((>=)goal) presents
            |> List.map (fun (p:int) ->
                List.where ((>)p) presents
                |> combinations (goal - p) 
                |> List.where (List.forall ((<)0))
                |> List.map (fun e -> p::e))
            |> List.concat
        
    let sum = List.sum presents
    let goal = sum / parts
    
    combinations goal presents
    |> List.map (fun x -> (List.length x, List.fold (fun t x -> (int64 x) * t) 1L x))
    |> List.sort
    |> List.head
    |> snd
let testInput = [1; 2; 3; 4; 5; 7; 8; 9; 10; 11]

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadAllLines("input.txt")
        |> Array.map (int)
        |> Array.toList
        
    findIdealConfiguration 3 input |> printfn "Part1: %d"
    findIdealConfiguration 4 input |> printfn "Part2: %d"

    0 // return an integer exit code