open System.IO

let hexToBinary c =
    match c with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> failwith "unknown char"

let binToDec bin =
    (0L, bin)
    ||> Seq.fold (fun v b ->
        match b with
        | '0' -> 0L
        | '1' -> 1L
        | _ -> failwith "unknown binary"
        |> fun x -> 2L*v + x )

let read bits packet =
    packet
    |> Seq.take bits
    |> Seq.toList
    |> binToDec, Seq.skip bits packet

let readNumber packet =
    let rec fetch input =
        let group = input |> Seq.take 5
        let rest = input |> Seq.skip 5
        let value = Seq.tail group

        match Seq.head group with
        | '0' -> value, rest
        | '1' -> let f, rest = fetch rest
                 Seq.append value f, rest

        | _ -> failwith "not allowed group header"

    let v, rest = fetch packet
    binToDec v, rest


let parse (input: string) =
    input
    |> Seq.map hexToBinary
    |> Seq.concat


type Packet =
    | Empty
    | Literal of int64 * int64
    | Operator of int64 * int64 * Packet list

let rec readPacket binaryInput =

    let version, rest = read 3 binaryInput
    let packetType, rest = read 3 rest
    if  Seq.forall ((=) '0') binaryInput then Empty, binaryInput else

    let rec readPackets input =
        match Seq.isEmpty input with
        | true -> [], input
        | _ ->
            let p, rest = readPacket input
            match p with
            | Empty -> [], input
            | x ->
                let packets, rest = readPackets rest
                x::packets, rest

    match packetType with
    | 4L ->
        let value, rest = readNumber rest
        Literal (version, value), rest
    | _ ->
        let lengthType, rest = read 1 rest
        match lengthType with
        | 0L ->
            let length, rest = read 15 rest
            let subPackets = Seq.take (int length) rest
            let childPackets, _ = readPackets subPackets
            Operator (version, packetType, childPackets), rest |> Seq.skip (int length)
        | 1L ->
            let packetsCount, rest = read 11 rest
            let childPackets, rest =
                [1L..packetsCount]
                |> List.fold (fun (packets, rest) _ ->
                    let p, rest = readPacket rest
                    p::packets, rest
                ) ([], rest)
            Operator (version, packetType, List.rev childPackets), rest
        | _ -> failwith "unknown length type"

let rec versionSum packet =
    match packet with
    | Empty -> 0L
    | Literal(version, value) -> version
    | Operator(version, _, packets) -> version + List.sumBy (versionSum) packets

let part1 (packet, rest) =
    versionSum packet

let rec evaluate packet =
    match packet with
    | Empty -> 0L
    | Literal(_, value) -> value
    | Operator(_, 0L, packets) -> List.map evaluate packets |> List.sum
    | Operator(_, 1L, packets) -> List.map evaluate packets |> List.reduce (*)
    | Operator(_, 2L, packets) -> List.map evaluate packets |> List.min
    | Operator(_, 3L, packets) -> List.map evaluate packets |> List.max
    | Operator(_, 5L, [a; b]) -> if evaluate a > evaluate b then 1 else 0
    | Operator(_, 6L, [a; b]) -> if evaluate a < evaluate b then 1 else 0
    | Operator(_, 7L, [a; b]) -> if evaluate a = evaluate b then 1 else 0
    | _ -> failwith "unkown packet"

let part2 (packet, rest) =
    evaluate packet

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")

    let packet = input |> parse |> readPacket
    packet |> part1 |> printfn "Part 1: %d"
    packet |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
