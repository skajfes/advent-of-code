open System.IO

let parse (input: string[]) = 
    input
    |> Array.mapi (fun r row ->
        row
        |> Seq.mapi (fun c plant -> (r, c), plant))
    |> Seq.concat
    |> Seq.toList

let ns (r, c) =
    [ (r-1, c); (r+1, c); (r, c+1); (r, c-1) ]

let contains plant coord regions =
    let neighbours = ns coord
    regions
    |> List.partition (fun (r_plant, coords) ->
        r_plant = plant &&
        coords
        |> List.exists (fun c -> List.contains c neighbours)
        )

let find_areas map =
    map
    |> List.fold (
        fun regions (coord, plant) ->
            match contains plant coord regions with
            | [], rs -> (plant, [coord])::rs
            | [r_plant, r_coords], rs -> (plant, coord::r_coords)::rs
            | m_regions, rs -> (plant, coord::(m_regions |> List.map snd |> List.concat))::rs
            | _ -> failwith "nope"
    ) []
    |> List.map (fun (plant, coords) ->
        let area = coords |> List.length
        let circ = coords
                   |> List.map (fun c -> ns c
                                         |> List.filter (fun n -> coords
                                                                  |> List.contains n)
                                         |> List.length)
                   |> List.map (fun c -> 4 - c)
                   |> List.sum

        let sides_up =
            coords
            |> List.map (fun (cr, cc) -> (cr, cc), [ cr - 1, cc; cr + 1, cc ] |> List.map (fun c -> List.contains c coords |> not))
            |> List.filter (fun ((cr, cc), [up; down]) -> up = true)
            |> List.map fst
            |> List.groupBy fst
            |> List.map (fun (r, coords) -> r, coords |> List.map snd |> List.sort |> List.pairwise |> List.map (fun (a, b) -> b - a) |> List.filter ((<) 1) |> List.length |> (+) 1)
            |> List.sumBy snd

        let sides_down =
            coords
            |> List.map (fun (cr, cc) -> (cr, cc), [ cr - 1, cc; cr + 1, cc ] |> List.map (fun c -> List.contains c coords |> not))
            |> List.filter (fun ((cr, cc), [up; down]) -> down = true)
            |> List.map fst
            |> List.groupBy fst
            |> List.map (fun (r, coords) -> r, coords |> List.map snd |> List.sort |> List.pairwise |> List.map (fun (a, b) -> b - a) |> List.filter ((<) 1) |> List.length |> (+) 1)
            |> List.sumBy snd
        plant, area, circ, 2 * (sides_up + sides_down) // there is same number of vertical and horizontal sides
        )

let part1 areas =
    areas
    |> List.sumBy (fun (plant, area, circumference, sides) -> area * circumference)

let part2 areas =
    areas
    |> List.sumBy (fun (plant, area, circumference, sides) -> area * sides)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> find_areas |> part1 |> printfn "%A"
    testInput |> parse |> find_areas |> part2 |> printfn "%A"

    let areas = input |> parse |> find_areas
    part1 areas |> printfn "Part 1: %A"
    part2 areas |> printfn "Part 2: %A"

    0 // return an integer exit code
