open System
open System.IO

type BoardNumber =
    | Num of int
    | Mark of int

let parse (input: string[]) =
    let numbers =
        input.[0].Split(',')
        |> Array.map int

    let boards =
        input.[2..]
        |> Array.filter (fun a -> a <> "")
        |> Array.chunkBySize 5
        |> Array.map (fun board ->
               board
               |> Array.map (fun r ->
                   r.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                   |> Array.map int
                   |> Array.map Num))

    numbers, boards


let markNumber draw board =
    board
    |> Array.map (fun r ->
        r |> Array.map (function
            | Num x when x = draw -> Mark x
            | x -> x))

let winner board =
    let hasRow board =
        board
        |> Array.map (fun r -> Array.sumBy (function | Mark _ -> 1 | _ -> 0) r)
        |> Array.exists ((=) 5)

    hasRow board || hasRow (Array.transpose board)

let score board draw =
    board
    |> Array.concat
    |> Array.sumBy (function
        | Num x -> x
        | _ -> 0)
    |> (*)draw

let findWinner last (numbers, boards) =
    ((boards, None), numbers)
    ||> Seq.fold (fun (boards, winScore) draw ->
            match winScore with
            | Some _ -> boards, winScore
            | _ ->

            let boards = boards |> Array.map (markNumber draw)

            if last && Array.length boards > 1 then
                let boards = Array.filter (fun b -> winner b |> not) boards
                boards, None
            else
            match boards
                |> Array.filter winner
                |> Seq.tryHead
                with
                | Some board -> Array.empty, Some (score board draw)
                | None -> boards, None

        )
    |> snd

let part1 input =
    findWinner false input

let part2 input =
    findWinner true input


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    //testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
