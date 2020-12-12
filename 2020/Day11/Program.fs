open System
open System.IO

type Seat =
    | Empty
    | Occupied
    | Floor

let parse input =
    input
    |> Seq.mapi (fun r row ->
        Seq.mapi (fun c ->
            function
            | 'L' -> Empty
            | '#' -> Occupied
            | '.' -> Floor
            | _ -> failwith "none") row |> Seq.toArray)
    |> Seq.toArray

let printmap (map: Seat [][]) =
    Console.SetCursorPosition(0, 0)
    map
    |> Array.map (
         Array.map (function
                   | Empty -> "L"
                   | Occupied -> "#"
                   | Floor -> ".")
         >> String.Concat
         >> fun s -> s + "\n")
    |> String.Concat
    |> printfn "%s"
    
    Threading.Thread.Sleep(100)


let neighbours max_r max_c r c (map: Seat[][]) =
    [|(r - 1, c - 1); (r - 1, c); (r - 1, c + 1)
      (r, c - 1);                 (r, c + 1)
      (r + 1, c - 1); (r + 1, c); (r + 1, c + 1)|]
    |> Array.filter (fun (r, c) -> r >= 0 && c >= 0 && r < max_r && c < max_c)
    |> Array.fold (fun n (r, c) ->
        match map.[r].[c] with
        | Occupied -> n + 1
        | _ -> n) 0
            
let neighbours' r c map =
    let direction f_row f_col =
        seq { 1 .. 100000 }
        |> Seq.map (fun d ->
             Array.tryItem (f_row r d) map
             |> function
                | None -> None
                | Some row ->
                    Array.tryItem (f_col c d) row)
        |> Seq.filter (
            function
            | Some Floor -> false
            | _ -> true)
        |> Seq.head
        |> function
            | Some Occupied -> 1
            | _ -> 0
        
    let inc a b = a + b
    let dec a b = a - b
    let id a b = a
    
    [| (id, inc); (id, dec ); (dec, id); (inc, id)
       (dec, inc); (dec, dec); (inc, inc); (inc, dec) |]
    |> Array.sumBy (fun (r, c) -> direction r c)
            
    
let round neighbours limit map =
    Array.mapi (fun r ->
        Array.mapi (fun c seat -> 
            match seat, neighbours r c map with
            | Empty, 0 -> Occupied
            | Occupied, cnt when cnt >= limit -> Empty
            | s, _ -> s)) map

let game neighbours limit map =
    let rec playRound map =
        let map' = round neighbours limit map
//        printmap map
        if map' = map then map else playRound map'

//    printmap map
    playRound map
    |> Array.fold (
        Array.fold (fun sum ->
            function
            | Occupied -> sum + 1
            | _ -> sum)) 0


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let map = parse input
    let max_r = map.Length
    let max_c = map.[0].Length
    map |> game (neighbours max_r max_c) 4 |> printfn "Part 1: %d"
    map |> game neighbours' 5 |> printfn "Part 2: %d"

    0 // return an integer exit code
