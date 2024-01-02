open System.IO

let parse (input: string[]) =
    let coord (x: string) = x.Split(',') |> Array.map int |> fun [| a; b; c |] -> a, b, c
    input
    |> Array.map (fun l -> l.Split('~') |> fun [|a; b|] -> (coord a, coord b))

let lower (((x1: int, y1: int, z1: int), (x2: int, y2: int, z2: int))) =
    ((x1, y1, z1 - 1), (x2, y2, z2 - 1))

let out_of_bounds ((x1: int, y1: int, z1: int), (x2: int, y2: int, z2: int)) =
    z1 < 1 || z2 < 1

let intersects ((ax1: int, ay1: int, az1: int), (ax2: int, ay2: int, az2: int)) ((bx1: int, by1: int, bz1: int), (bx2: int, by2: int, bz2: int)) =
    // cube a not fallen to reach b
    if az1 > bz2 then false
    // cube a fallen past b
    elif az2 < bz1 then false
    // cube a on same height as b
    // cube a entirely to the left of b
    elif ax2 < bx1 then false
    // cube a entirely to the right of b
    elif ax1 > bx2 then false
    // cube a in front of b
    elif ay1 > by2 then false
    // cube a behind b
    else not (ay2 < by1)

let intersecting cubes cube =
    cubes
    |> List.filter (fst >> intersects cube)
    |> List.map fst

let rec fall_down cubes cube =
    let cube' = lower cube
    let intersect = intersecting cubes cube'
    if out_of_bounds cube' then
        cube, []
    elif intersect <> List.empty then
        cube, intersect
    else
        fall_down cubes cube'

let gravity cubes =
    cubes
    |> Array.sortBy (fun ((_, _, z1), (_, _, z2)) -> min z1 z2)
    |> Array.fold (fun cubes cube ->
        // printfn "cube %A" cube
        (fall_down cubes cube)::cubes
        ) []
    |> List.toArray

let same cubes cubes' =
    cubes
    |> Array.forall (fun x -> cubes' |> Array.contains x)

let can_disintegrate cubes =
    let supports =
        cubes
        |> Array.map fst
        |> Array.map (fun cube -> cube, cubes |> Array.filter (fun (c, ds) -> ds |> List.contains cube) |> Array.map fst)

    let cs = cubes |> Map.ofArray
    supports
    |> Array.filter (fun (_, s) -> s = Array.empty || s |> Array.filter (fun c -> (cs |> Map.find c |> List.length) = 1) = Array.empty)
    |> Array.map fst

let chain_reaction cubes =
    let supports =
        cubes
        |> Array.map fst
        |> Array.map (fun cube -> cube, cubes |> Array.filter (fun (c, ds) -> ds |> List.contains cube) |> Array.map fst)
        |> Map.ofArray

    let depends =
        cubes
        |> Map.ofArray

    let rec count_affected depends cube =
        supports
        |> Map.find cube
        |> Array.fold (fun (cnt, depends) c ->
            depends
            |> Map.find c
            |> List.filter ((<>)cube)
            |> function
                | [] ->
                    // all supports removed, collapse this block as well
                    let depends = depends |> Map.add c []
                    let cnt', depends = count_affected depends c
                    (cnt + cnt' + 1), depends
                | ds ->
                    // has more supports, just remove current
                    cnt, depends |> Map.add c ds
            ) (0, depends)

    cubes
    |> Array.map (fst >> count_affected depends)

let part1 cubes =
    cubes
    |> can_disintegrate
    |> Array.length

let part2 cubes =
    cubes
    |> chain_reaction
    |> Array.sumBy fst

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    let testParsed = testInput |> parse |> gravity
    testParsed |> part1 |> printfn "%A"
    testParsed |> part2 |> printfn "%A"

    let parsed = input |> parse |> gravity
    parsed |> part1 |> printfn "Part 1: %A"
    parsed |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
