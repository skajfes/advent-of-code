open System.IO

type Direction = Left | Right

let parse (input: string[]) =
    input
    |> Array.map (fun line ->
                    let dir = match line[0] with
                                | 'L' -> Left
                                | 'R' -> Right
                                | _ -> failwith "Invalid direction"
                    dir, (line[1..] |> int))

let part1 dirs =
    ((50, []), dirs)
    ||> Array.fold (fun (dir, states) (d, s) ->
                     let f op = (op dir s) % 100
                     match d with
                     | Left -> f (-), (f (-))::states
                     | Right -> f (+), (f (+))::states)
    |> snd
    |> List.filter ((=) 0)
    |> List.length

let part2 dirs =
    ((50, 0), dirs)
    ||> Array.fold (fun (dir, count) (d, s) ->
            let dir' = match d with Left ->  dir - s | Right -> dir + s

            let c = dir' / 100 |> abs
            let dir'' = dir' % 100
            // let dir'' = if dir'' < 0 then dir'' + 100 else dir''
            //
            // let extra = if not ((dir'' < 0 && dir = 0)) && (dir'' < 0) || (dir''  = 0 && d = Left) then 1 else 0
            if (dir'' < 0 && dir = 0) then
                (100 + dir'', count + c)
            elif (dir'' < 0) then
                (100 + dir'', count + c + 1)
            elif (dir'' = 0 && d = Left) then
                (dir'', count + c + 1)
            else
                (dir'', count + c)
            // (dir'', count + c + extra)

        )
    |> snd

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part2 |> printfn "%A"
    
    // input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
