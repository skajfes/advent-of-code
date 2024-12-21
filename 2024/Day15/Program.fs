open System
open System.IO

type Field =
    | Wall of (int*int)
    | Robot of (int*int)
    | Box of (int*int)

let parse_map input =
    input
    |> Array.mapi (fun r row ->
        row
        |> Seq.mapi (fun c ->
            function
            | '#' -> Some (Wall (r, c))
            | '@' -> Some (Robot (r, c))
            | 'O' -> Some (Box (r, c))
            | _ -> None)
        )
    |> Seq.concat
    |> Seq.filter (function Some x -> true | _ -> false)
    |> Seq.map (fun (Some x) -> x)
    |> Seq.toArray

type Move = Up | Down | Left | Right

let parse_moves input =
    input
    |> Array.map (fun r ->
        r
        |> Seq.map (
            function
            | '>' -> Right
            | '<' -> Left
            | 'v' -> Down
            | '^' -> Up
            | _ -> failwith "nope"))
    |> Seq.concat
    |> Seq.toList

let parse (input: string[]) =
    input
    |> Array.splitAt (Array.findIndex ((=)"") input)
    |> fun (map, moves) -> parse_map map, parse_moves moves

let coord =
    function
    | Robot c -> c
    | Wall c -> c
    | Box c -> c

let set_coord field c =
    match field with
    | Robot _ -> Robot c
    | Wall _ -> Wall c
    | Box _ -> Box c

let draw map =
    let n = map |> Array.map coord |> Array.map fst |> Array.max

    [for r in 0..n do
        seq [for c in 0..n -> match Array.tryFind (fun x -> coord x = (r, c)) map with
                                   | Some (Robot _) -> "@"
                                   | Some (Wall _) -> "#"
                                   | Some (Box _) -> "O"
                                   | None -> " "
                                   ]
        |> String.concat ""]
    |> String.concat "\n"

let draw3 walls boxes robot =
    let n = walls |> List.map fst |> List.max

    [for r in 0..n do
        seq [for c in 0..2*n+1 ->
               if List.exists (fun (wr, wc) -> (wr, wc) = (r, c) || (wr, wc+1) = (r, c)) walls then "#"
               elif List.exists (fun (br, bc) -> (br, bc) = (r, c)) boxes then "["
               elif List.exists (fun (br, bc) -> (br, bc + 1) = (r, c)) boxes then "]"
               elif (fst robot) = r && (snd robot) = c then "@"
               else " "
        ]
        |> String.concat ""]
    |> String.concat "\n"

let go_dir (r, c) m =
    match m with
    | Up -> r - 1, c
    | Down -> r + 1, c
    | Right -> r, c + 1
    | Left -> r, c - 1

let rec move field dir (map: Field array) =
    let target = (go_dir (coord field) dir)
    match Array.tryFind (fun x -> coord x = target) map with
    | None ->
        true, map
              |> Array.filter (fun x -> x <> field)
              |> Array.append [| set_coord field target |]
    | Some (Wall _) -> false, map
    | Some (Robot c) -> failwith ""
    | Some (Box c) ->
        match move (Box c) dir map with
        | false, map -> false, map
        | true, map ->
            true, map |> Array.map (fun f -> if f = field then (set_coord field target) else f)

let rec do_moves (Robot (rr, rc)) (map: Field array) moves =
    match moves with
    | [] -> map
    | m::ms ->
        let map = move (Robot (rr, rc)) m map |> snd
        let robot = map |> Array.find (function Robot _ -> true | _ -> false)
        // draw map |> printfn "%s"
        // Console.ReadKey() |> ignore
        do_moves robot map ms


let calculate_gps map =
    map
    |> Array.map (
        function
        | Box (r, c) -> 100*r + c
        | _ -> 0)
    |> Array.sum

let calculate_gps2 boxes =
    boxes
    |> List.map (fun (r, c) -> 100*r + c)
    |> List.sum

let part1 (map, moves) =
    let robot = map |> Array.find (function Robot _ -> true | _ -> false)
    do_moves robot map moves
    |> calculate_gps

let double_map map =
    map
    |> Array.map (fun f ->
        match f with
        | Robot (r, c) -> Robot (r, c*2)
        | Wall (r, c) -> Wall (r, c*2)
        | Box (r, c) -> Box (r, c*2)
        )

let new_coord m (r, c) =
    match m with
    | Up -> r - 1, c
    | Down -> r + 1, c
    | Left -> r, c - 1
    | Right -> r, c + 1

let do_moves3 (walls, boxes, robot) moves =

    let rec move_box boxes box m =
        match boxes with
        | None -> None
        | Some boxes ->

            let b_new = new_coord m box
            // printfn "Moving box from %A to %A" box b_new

            let pushed_walls =
                walls
                |> List.exists (fun (wr, wc) ->
                    (wr, wc) = b_new ||
                    (wr, wc + 1) = b_new ||
                    (wr, wc) = (fst b_new, snd b_new + 1) ||
                    (wr, wc + 1) = (fst b_new, snd b_new + 1)
                )

            if pushed_walls then None else

            let pushed_boxes =
                boxes
                |> List.filter (fun (br, bc) ->
                    (br, bc) <> box &&
                        ((br, bc) = b_new ||
                        (br, bc + 1) = b_new ||
                        (br, bc) = (fst b_new, snd b_new + 1) ||
                        (br, bc + 1) = (fst b_new, snd b_new + 1)) // can't happen?
                )

            pushed_boxes
            |> List.fold (fun boxes box -> move_box boxes box m) (Some boxes)
            |> function
                | None -> None
                | Some boxes ->
                    boxes
                    |> List.filter (fun b -> b <> box)
                    |> List.append [b_new]
                    |> Some

    let move_robot boxes (r, c) m =
        let r_new = new_coord m (r, c)
        // printfn "Pushing robot from %A to %A" (r, c) r_new

        let pushed_walls =
            walls
            |> List.exists (fun (wr, wc) -> (wr, wc) = r_new || (wr, wc + 1) = r_new)

        if pushed_walls then boxes, (r, c) else

        let pushed_boxes =
            boxes
            |> List.filter (fun (br, bc) -> (br, bc) = r_new || (br, bc + 1) = r_new)

        pushed_boxes
        |> List.fold (fun boxes box ->
             move_box boxes box m ) (Some boxes)
        |> function
            | Some boxes ->
                // draw3 walls boxes r_new |> printfn "%s"
                // Console.ReadKey() |> ignore
                boxes, r_new // success - move robot
            | None -> boxes, (r, c)// failure - don't move robot

    moves
    |> List.fold (fun (boxes, robot) m -> move_robot boxes robot m) (boxes, robot)
    |> fst



let part2 (map, moves) =
    let (walls, boxes, robot) =
        map
        |> double_map
        |> Array.fold (fun (walls, boxes, robot) ->
            function
            | Robot c -> walls, boxes, c
            | Wall w -> w::walls, boxes, robot
            | Box b -> walls, b::boxes, robot
        ) ([], [], (0,0))
    do_moves3 (walls, boxes, robot) moves
    |> calculate_gps2

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
