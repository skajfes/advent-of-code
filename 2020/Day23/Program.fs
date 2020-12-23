open System.Diagnostics
let parse (input: string) =
    input |> Seq.toArray |> Array.map (string >> int)

type Node(value) =
    let mutable next = None
    let mutable smaller = None

    member this.setNext(n) =
        next <- Some n
    member this.clearNext() =
        next <- None

    member this.setSmaller(n) =
        smaller <- Some n

    member this.Value = value
    member this.Next =
        match next with
        | Some node -> node
        | _ -> failwith "not supposed to be empty"
    member this.Smaller =
        match smaller with
        | Some node -> node
        | _ -> failwith "not supposed to be empty"

    member this.SmallerSafe =
        match smaller:Node option with
        | Some node -> node.Value
        | _ -> -1

    member this.IsLast =
        match next with
        | Some node -> false
        | None -> true

    member this.Flatten() =
        let rec ff (i:Node) =
            match i.Value, i.IsLast with
            | v, _ when v = this.Value -> []
            | v, true -> [v]
            | v, false -> v :: ff i.Next

        this.Value :: ff this.Next



type SuperLinkedList(items) =
    let start = Node(Array.item 0 items)

    do
        let rest = items.[1..]
        let nodes =
            (Map.empty, [1..10])
            ||> List.fold (fun nodes i ->
                Map.add i (Node(i)) nodes )
            |> Map.add (start.Value) start

        if start.Value > 1 then
            let prev = Map.find (start.Value - 1) nodes
            start.setSmaller(prev)

        let rest_nodes =
            Array.foldBack (fun v (next: Node) ->
                let n = if Map.containsKey v nodes then Map.find v nodes else Node (v)

                n.setNext(next)
                if v = 10 then
                    let nine = Map.find 9 nodes
                    n.setSmaller(nine)
                    next.setSmaller(n)
                else if v < 10 && v > 1 then
                    let smaller = Map.find (v - 1) nodes
                    n.setSmaller(smaller)
                else if next = start then
                    let one = Map.find 1 nodes
                    one.setSmaller(n)
                else
                    next.setSmaller(n)
                n) rest start

        start.setNext(rest_nodes)
        let one = Map.find 1 nodes
        if one.SmallerSafe = -1 then
            let nine = Map.find 9 nodes
            one.setSmaller(nine)

    member self.First = start

let playGame totalRounds cups =
    let d = Array.length cups
    let linked_cups = SuperLinkedList(cups)

    let rec getDestValue dest (moving: Node) =
        let dest = if dest <= 0 then d else dest
        if moving.Value = dest || moving.Next.Value = dest || moving.Next.Next.Value = dest
        then getDestValue (dest - 1) moving
        else dest

    let rec findDest value (item: Node) =
        if item.Value = value then item
        else findDest value (item.Smaller)

    let rec playRound round (current: Node) =
        // if round % 100000 = 0 then printfn "Round %d" round
        if round > totalRounds then current else
        let moving = current.Next
        let end_moving = moving.Next.Next
        current.setNext(end_moving.Next)
        end_moving.clearNext()
        let dest_value = getDestValue (current.Value - 1) moving
        let dest = findDest dest_value current
        let rest = dest.Next
        end_moving.setNext(rest)
        dest.setNext(moving)
        playRound (round + 1) current.Next

    playRound 1 linked_cups.First
    |> findDest 1

let smallGame cups =
    playGame 100 cups
    |> fun x -> x.Flatten()
    |> List.tail
    |> List.map string
    |> String.concat ""

let moreCups cups =
    Array.append cups [|Array.length cups + 1..1_000_000|]

let realGame cups =
    let res = playGame 10_000_000 cups
    int64 res.Next.Value * int64 res.Next.Next.Value


[<EntryPoint>]
let main argv =
    let input = "167248359"
    let testInput = "389125467"

    let sw = Stopwatch.StartNew()

    input |> parse |> smallGame |> printfn "Part 1: %s"
    let p1 = sw.ElapsedMilliseconds
    sw.Restart()
    input |> parse |> moreCups |> realGame |> printfn "Part 2: %d"
    let p2 = sw.ElapsedMilliseconds

    printfn "Part1: %d ms, Part2 %d ms" p1 p2

    0 // return an integer exit code