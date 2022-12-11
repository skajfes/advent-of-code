open System.IO

let parse (input: string[]) =
    input
    |> Array.map (fun row -> Seq.map (string >> int) row |> Seq.toArray)
    |> array2D

let visible (map: int[,]) r c tree =
    if r = 0 || c = 0 || r = Array2D.length1 map - 1 || c = Array2D.length2 map - 1 then true else
    let visibleUp =
        [ 0 .. r - 1 ]
        |> List.map (fun r -> map.[r, c] < tree)
        |> List.forall ((=) true)

    let visibleDown =
        [ r + 1 .. Array2D.length1 map - 1 ]
        |> List.map (fun r -> map.[r, c] < tree)
        |> List.forall ((=) true)

    let visibleLeft =
        [ 0 .. c - 1 ]
        |> List.map (fun c -> map.[r, c] < tree)
        |> List.forall ((=) true)

    let visibleRight =
        [ c + 1 .. Array2D.length2 map - 1 ]
        |> List.map (fun c -> map.[r, c] < tree)
        |> List.forall ((=) true)

    visibleUp || visibleDown || visibleLeft || visibleRight

let visible2 (map: int[,]) r c tree =
    if r = 0 || c = 0 || r = Array2D.length1 map - 1 || c = Array2D.length2 map - 1 then 0 else

    let visibleDir coords =
        coords
        |> List.map (fun (r, c) -> map.[r, c])
        |> List.tryFindIndex (fun x -> x >= tree)
        |> function
            | None -> List.length coords
            | Some x -> x + 1

    let visibleUp =
        [ for r in r - 1 .. -1 .. 0 do r, c]
        |> visibleDir

    let visibleDown =
        [ for r in r + 1 .. Array2D.length1 map - 1 do r, c]
        |> visibleDir

    let visibleLeft =
        [ for c in c - 1  .. -1 .. 0 do r, c]
        |> visibleDir

    let visibleRight =
        [ for c in c + 1 .. Array2D.length2 map - 1 do r, c]
        |> visibleDir

    visibleUp * visibleDown * visibleLeft * visibleRight
    // visibleUp, visibleDown, visibleLeft, visibleRight

let part1 map =
    map
    |> Array2D.mapi (visible map)
    |> Seq.cast<bool>
    |> Seq.filter ((=) true)
    |> Seq.length

let part2 map =
    map |> Array2D.mapi (visible2 map)
    |> Seq.cast<int>
    |> Seq.max

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
