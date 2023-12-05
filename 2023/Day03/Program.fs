open System
open System.IO
open Microsoft.FSharp.Collections

type Parts =
    | Number of string
    | Symbol
    | Gear

let numbers (coords: ((int*int)*char) array array) =
    coords
    |> Array.toList
    |> List.map (Array.fold (fun (number_so_far, first_coord, found) (coord, v) ->
        match v with
        | c when List.contains c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] && number_so_far <> "" -> number_so_far + string c, first_coord, found
        | c when List.contains c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] -> number_so_far + string c, coord, found
        | '.' when number_so_far <> "" -> "", (-1, -1), (first_coord, Number number_so_far)::found
        | '.' -> "", (-1, -1), found
        | '*' when number_so_far <> "" -> "", (-1, -1), (coord, Gear)::(first_coord, Number number_so_far)::found
        | '*' -> "", (-1, -1), (coord, Gear)::found
        | s when number_so_far <> "" -> "", (-1, -1), (coord, Symbol)::(first_coord, Number number_so_far)::found
        | s -> "", (-1, -1), (coord, Symbol)::found
        ) ("", (-1, -1), []))
    |> List.map (fun (number_so_far, last_coord, found) -> if number_so_far <> "" then (last_coord, Number number_so_far)::found else found)
    |> List.concat

let parse (input: string[]) = 
    input
    |> Array.mapi (fun i x -> x |> Seq.mapi (fun j y -> (i, j), y) |> Seq.toArray)
    |> numbers


let allNeighbours (x, y) len =
    // x-1 y: y-1..y+len+2
    // x y-1 y+1
    // x+1 y. y-1..y+len+2
    seq {
        yield! [for dy in -1..len -> (x-1, y+dy)]
        yield! [for dy in -1..len -> (x+1, y+dy)]
        yield! [x, y-1; x, y+len]
    }
    |> List.ofSeq

let adjacentSymbol coord len symbols =
    let ns = allNeighbours coord len
    symbols
    |> List.filter (fun c -> List.contains c ns)
    |> List.isEmpty
    |> not

let adjacentNumbers numbers coord =
    numbers
    |> List.filter (fun (cs, v) -> List.contains coord cs)
    |> List.map snd

let part1 coords =
    let symbols = coords
                  |> List.filter (snd >> function Symbol -> true | Gear -> true | _ -> false)
                  |> List.map fst
    ([], coords)
    ||> List.fold (fun a (coord, p) ->
        match p with
        | Number x when adjacentSymbol coord (String.length x) symbols -> int x::a
        | Number x -> a
        | Gear -> a
        | Symbol -> a )
    |> List.sum

let part2 coords =
    let gears = coords |> List.filter (fun c -> snd c = Gear) |> List.map fst
    let numbers = coords
                     |> List.filter (snd >> function Number _ -> true | _ -> false)
                     |> List.map (fun (c, v) -> match v with Number x -> allNeighbours c (String.length x), int x | _ -> failwith "nope")

    gears
    |> List.map (adjacentNumbers numbers)
    |> List.map (function
                | [ a; b ] -> a * b
                | _ -> 0)
    |> List.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part2 |>  printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
