open System.IO

let parse (input: string[]) = 
    ([], input)
    ||> Array.fold (fun elves food ->
        match elves, food with
        | elves, "" -> []::elves
        | [], _ -> [(int food)]::[]
        | elf::elves, _ -> ((int food)::elf)::elves
        )
    |> List.rev

let part1 (elves: int list list) =
    elves
    |> List.map (List.sum)
    |> List.max

let part2 (elves: int list list) =
    elves
    |> List.map (List.sum)
    |> List.sortDescending
    |> List.take 3
    |> List.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
