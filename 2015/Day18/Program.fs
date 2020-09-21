open System

let parse (input: string) =
    input.Split('\n')
    |> Array.map (fun r ->
        Seq.map (function
            | '#' -> 1
            | _ -> 0) r
        |> Seq.toList)
    |> Array.toList
    
    
let aliveNeighbours i j (board: int list list)=
    [(i-1, j-1); (i-1, j); (i-1, j+1)
     (i, j-1);             (i, j+1)
     (i+1, j-1); (i+1, j); (i+1, j+1)]
    |> List.map (fun (i, j) ->
        match List.tryItem i board with
        | None -> 0
        | Some r ->
            match List.tryItem j r with
            | Some 1 -> 1
            | _ -> 0)
    |> List.sum
    
let generation board =
    List.mapi (fun i r ->
        List.mapi (fun j cell ->
            match cell, aliveNeighbours i j board with
            | 0, 3 -> 1
            | 1, 2 -> 1
            | 1, 3 -> 1
            | _ -> 0) r) board
    
let rec generations count f board =
    let board = f board
    if count = 0
    then board
    else
        generation board
        |> generations (count - 1) f
    
let cornersAreStuck board =
    List.mapi (fun i r ->
        List.mapi (fun j cell ->
            match i, j with
            | 0, 0 | 99, 0 | 0, 99 | 99, 99 -> 1
            | _ -> cell) r) board 
    
let countLights board =
    List.fold (fun sum r ->
        List.fold (fun sum c -> sum + c) sum r) 0 board
    
[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllText("input.txt") |> parse
    input |> generations 100 id |> countLights |> printfn "part1: %A lights are on"
    input |> generations 100 cornersAreStuck |> countLights |> printfn "part2: %A lights are on with stuck lights"
    
    
    0 // return an integer exit code
