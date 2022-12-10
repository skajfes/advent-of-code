open System.IO


let parseStacks input =
    let len =
        input |> Array.map String.length |> Array.max

    input
    |> Array.map (fun s -> s + (String.replicate 100 " ") |> Seq.take len)
    |> Array.map Seq.toArray
    |> Array.transpose
    |> Array.map Array.rev
    |> Array.filter (fun a -> Array.head a |> System.Char.IsNumber)
    |> Array.fold (fun map stack ->
        let key = (Array.head stack |> string |> int)
        let value = (Array.tail stack |> Array.rev |> Array.filter ((<>)' ')|> Array.toList)
        map |> Map.add key value) Map.empty

let parseCommands input =
    input
    |> Array.filter ((<>)"")
    |> Array.map (fun (s: string) ->
            let w = s.Split(" ")
            int w.[1], int w.[3], int w.[5]
        )

let parse (input: string[]) =
    let empty = input |> Array.findIndex ((=)"")
    let (stacks, commands) = input |> Array.splitAt empty

    (parseStacks stacks, parseCommands commands)


let rec stack (n, a, b) stacks =
    match n with
    | 0 -> stacks
    | n ->
        let el::a_stack = Map.find a stacks
        let b_stack = Map.find b stacks
        stacks
        |> Map.add a a_stack
        |> Map.add b (el::b_stack)
        |> stack (n - 1, a, b)

let stack2 (n, a ,b) stacks =
    let a_stack = Map.find a stacks
    let b_stack = Map.find b stacks

    stacks
    |> Map.add a (List.skip n a_stack)
    |> Map.add b (List.append (List.take n a_stack) b_stack)


let part1 (stacks, commands) =
    commands
    |> Array.fold (fun stacks cmd -> stack cmd stacks) stacks
    |> Map.toList
    |> List.map (snd >> List.head >> string)
    |> String.concat ""

let part2 (stacks, commands) =
    commands
    |> Array.fold (fun stacks cmd -> stack2 cmd stacks) stacks
    |> Map.toList
    |> List.map (snd >> List.head >> string)
    |> String.concat ""

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %s"
    input |> parse |> part2 |> printfn "Part 2: %s"

    0 // return an integer exit code
