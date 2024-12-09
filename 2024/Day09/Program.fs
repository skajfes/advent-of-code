open System.IO

let expand line =
    line
    |> Seq.fold (fun (mem, free, pointer, use_mem) el ->

        let v = (string>>int64) el
        if use_mem then
            (pointer, v)::mem, free, pointer+v, false
        else
            mem, (pointer, v)::free, pointer+v, true

        ) ([], [], 0L, true)
    |> fun (mem, free, pointer, _) -> mem |> List.sort |> List.indexed |> List.rev, free |> List.sort

let parse (input: string[]) =
    input
    |> Array.head
    |> expand

let rec defrag mem free res =
    match mem with
    | [] -> res
    | (m_id, (m_pos, m_size))::mem ->
        match free |> Array.tryFindIndex (fun (f_pos, f_size) -> f_size > 0L && f_pos < m_pos) with
        | Some i ->
            let f_pos, f_size = free[i]
            // in place reduce amount of memory
            if f_size > m_size then
                free[i] <- (f_pos + m_size, f_size - m_size)
                defrag mem free ((m_id, (f_pos, m_size))::res)
            else
                free[i] <- (f_pos, 0)
                defrag ((m_id, (m_pos, m_size - f_size))::mem) free ((m_id, (f_pos, f_size))::res)

        | None ->
            defrag mem free ((m_id, (m_pos, m_size))::res)

let rec defrag2 mem free res =
    match mem with
    | [] -> res
    | (m_id, (m_pos, m_size))::mem ->
        match free |> Array.tryFindIndex (fun (f_pos, f_size) -> f_size >= m_size && f_pos < m_pos) with
        | Some i ->
            let f_pos, f_size = free[i]
            // in place reduce amount of memory
            free[i] <- (f_pos + m_size, f_size - m_size)
            defrag2 mem free ((m_id, (f_pos, m_size))::res)
        | None ->
            defrag2 mem free ((m_id, (m_pos, m_size))::res)

let checksum mem =
    mem
    |> List.collect (fun (id, (addr, size)) ->
        [for i in 0L..size-1L -> int64 id * (addr + i)])
    |> List.sum

let part1 (mem, free) =
    defrag mem (free |> List.toArray) []
    |> checksum

let part2 (mem, free) =
    defrag2 mem (free |> List.toArray) []
    |> checksum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
