open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some
        <| List.tail
            [ for g in m.Groups do
                  for c in g.Captures -> c.Value ]
    else
        None

type Monkey =
    { Id: int
      Items: int64 list
      Inspect: int64 -> int64
      Inspected: int
      Modulo: int64
      MonkeyTrue: int
      MonkeyFalse: int }

let monkey name =
    { Id = int name
      Items = []
      Inspect = id
      Inspected = 0
      Modulo = 0
      MonkeyTrue = 0
      MonkeyFalse = 0 }

let parse (input: string[]) =
    ([], input)
    ||> Array.fold (fun monkeys line ->
        match line, monkeys with
        | Regex "Monkey (\d+):" [ id ], _ -> monkey id :: monkeys
        | Regex "Starting items: (?:(\d+)(?:, )?)+" items, monkey :: monkeys -> { monkey with Items = items |> List.map int64 } :: monkeys
        | Regex "Operation: new = old \+ (\d+)" [ param ], monkey :: monkeys ->
            { monkey with Inspect = fun x -> x + (int64 param) } :: monkeys
        | Regex "Operation: new = old \* (\d+)" [ param ], monkey :: monkeys ->
            { monkey with Inspect = fun x -> x * (int64 param) } :: monkeys
        | Regex "Operation: new = old \* old" [], monkey :: monkeys -> { monkey with Inspect = fun x -> x * x } :: monkeys
        | Regex "Test: divisible by (\d+)" [ param ], monkey :: monkeys -> { monkey with Modulo = int param } :: monkeys
        | Regex "If true: throw to monkey (\d+)" [ m ], monkey :: monkeys -> { monkey with MonkeyTrue = int m } :: monkeys
        | Regex "If false: throw to monkey (\d+)" [ m ], monkey :: monkeys -> { monkey with MonkeyFalse = int m } :: monkeys
        | _ -> monkeys)
    |> List.rev


let addItem id item monkeys =
    monkeys
    |> List.map (fun m ->
        if m.Id = id then
            { m with Items = List.append m.Items [ item ] }
        else
            m)

let removeItems id monkeys =
    monkeys |> List.map (fun m -> if m.Id = id then { m with Items = [] } else m)

let increment_inspected id monkeys =
    monkeys
    |> List.map (fun m ->
        if m.Id = id then
            { m with Inspected = m.Inspected + 1 }
        else
            m)

let get_monkey id monkeys =
    monkeys |> List.filter (fun m -> m.Id = id) |> List.head

let turn worry monkeys monkey_id : Monkey list =
    let monkey = get_monkey monkey_id monkeys
    let modulos = monkeys |> List.fold (fun x m -> x * m.Modulo) 1L

    (monkeys, monkey.Items)
    ||> List.fold (fun monkeys item ->
        let item = (monkey.Inspect(item % modulos) / worry) % modulos
        let test = item % monkey.Modulo = 0
        let next = if test then monkey.MonkeyTrue else monkey.MonkeyFalse

        monkeys
        |> addItem next item
        |> increment_inspected monkey_id
        |> removeItems monkey_id)

let print r monkeys =
    printfn "Round: %d" r
    monkeys |> List.iter (fun m -> printfn "Monkey %d: %A" m.Id m.Items)
    System.Console.ReadKey() |> ignore
    monkeys

let game rounds worry monkeys =
    (monkeys, [ 1..rounds ])
    ||> List.fold (fun monkeys r ->
        (monkeys, [ 0 .. List.length monkeys - 1 ])
        ||> List.fold (turn worry)
        // |> print r
    )


let score rounds worry monkeys =
    monkeys
    |> game rounds worry
    |> List.map (fun m -> m.Inspected)
    |> List.sortDescending
    |> List.take 2
    |> fun [ a; b ] -> bigint a * bigint b

let part1 monkeys =
    score 20 3 monkeys

let part2 monkeys =
    score 10000 1 monkeys

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")

    // testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
