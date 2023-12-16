open System.IO

let parse (input: string[]) =
    let max_r = Array.length input
    let max_c = String.length (input[0])
    let platform =
        input
        |> Array.mapi (fun r row ->
            row
            |> Seq.mapi (fun c col ->
                (r, c), col)
            |> Seq.toArray)
        |> Array.concat
        |> Array.filter (snd >> (<>) '.')
    (platform, max_r, max_c)


let tilt_row row =
    ([], row |> Array.sort)
    ||> Array.fold (fun rocks rock ->
        match rock with
        | (r, c), '#' -> rock :: rocks
        | (r, c), 'O' ->
            match rocks with
            | [] -> [(0, c), 'O']
            | ((lastR, lastC), _)::rs -> ((lastR + 1, c), 'O') :: rocks
        | _ -> failwith "wat rock"
    )
    |> List.toArray

let load max_r ((r, c), rock) =
    match rock with
    | 'O' -> max_r - r
    | _ -> 0

let tilt_north (platform, max_r, max_c) =
    let tilted =
        platform
        |> Array.groupBy (fun ((r, c), _) -> c)
        |> Array.map snd
        |> Array.map tilt_row
        |> Array.concat
    tilted, max_r, max_c

let platform_load (p, max_r, _) = p |> Array.sumBy (load max_r)

let part1 (platform, max_r, max_c) =
    (platform, max_r, max_c)
    |> tilt_north
    |> platform_load

let rotate (platform, max_r, max_c) =
     let platform =
         platform
         |> Array.map (fun ((r, c), rock) -> (c, max_r - r - 1), rock)
     (platform, max_c, max_r)


let plat (a, b, c) = a
let part2 cycles platform =
    let rec do_cycle i platform =
        if i = 0 then platform else

        let p' = platform
                 |> (tilt_north >> rotate)
                 |> (tilt_north >> rotate)
                 |> (tilt_north >> rotate)
                 |> (tilt_north >> rotate)

        // printfn "%d %d" (cycles - i + 1) (platform_load p')

        do_cycle (i - 1) p'

    do_cycle cycles platform
    |> platform_load

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 1000 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"

    // manual solution
    // print out loads for first 200 spins
    // notice after initial set of cycles loads start repeating every 34 cycles
    // assume repeat starts after 150 spins
    // to get result after 1000000000 spins
    // 1000000000-150 = 999999850
    // 999999850 % 34 = 10
    // 150+10 spins for result
    input |> parse |> part2 160 |> printfn "Part 2: %A"

    0 // return an integer exit code
