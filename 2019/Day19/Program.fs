// Learn more about F# at http://fsharp.org

// #load "IntCode.fs"
open System

let drawMap map =
    let l = Map.toList map
    let minY = List.map (fst >> fst) l |> List.min
    let minX = List.map (fst >> snd) l |> List.min
    let maxY = List.map (fst >> fst) l |> List.max
    let maxX = List.map (fst >> snd) l |> List.max

    [for y in minY..maxY ->
        [for x in minX..maxX -> 
            match map.TryFind (x, y) with
            | Some 1L -> "#"
            | _ -> " "
        ] |> String.concat "" 
    ] |> String.concat "\n" |> printfn "%s"

[<EntryPoint>]
let main argv =
    
    let program = IO.File.ReadAllText("input.txt") |> IntCode.compile

    let map = 
        [for x in 0..39 do for y in 0..34 -> (x, y)]
        |> List.fold (fun map (x, y) -> 
            let _, output = IntCode.run [int64 x; int64 y] program
            Map.add (x, y) (List.head output) map
        ) Map.empty
    drawMap map

    // part 1
    printfn "part1: %d"
        (map 
        |> Map.toList
        |> List.filter (snd>>(=)1L)
        |> List.length)
    
    // part 2
    let check x y = IntCode.run [int64 x; int64 y] program |> snd |> List.head |> int

    let rec findBlock (x, y) d =
        let len = 99
        if d = 0 then (x+1 , y+1 - len, 10000 * (x+1) + (y + 1 - len)) else
        printfn "%A %d" (x, y - len) d
        
        match check x y with
        | 0 -> findBlock (x + 1, y) d
        | _ -> 
            match check x (y - len), check (x + len) (y - len) with
            | 0, _ -> findBlock (x, y + d) d
            | 1, 0 -> findBlock (x, y + d) d
            | 1, 1 -> findBlock (x - d, y - d) (d / 2)

    findBlock (0, 100) 800

    0 // return an integer exit code
