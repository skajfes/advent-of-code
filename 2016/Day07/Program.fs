open System.IO

let split (ip: string) =
    ip.Split([| '['; ']' |])
    |> Seq.indexed
    |> Seq.fold (fun (net, hypernet) (idx, part) ->
        match idx with
        | i when i % 2 = 0 -> (net + "|" + part, hypernet)
        | _ -> net, hypernet + "|" + part) ("", "")

let containsABBA'' (ip: string) =
    let rest, hypernet = split ip

    let check v =
        v
        |> Seq.windowed 4
        |> Seq.where (fun [|a; b; c; d|] -> a = d && b = c && a <> b)
        |> Seq.isEmpty
        |> not

    check rest && not (check hypernet)

let containsABA (ip: string) =
    let rest, hypernet = split ip

    rest
    |> Seq.windowed 3
    |> Seq.where (Array.contains '|' >> not)
    |> Seq.where (fun [|a; b; c;|] -> a = c && a <> b)
    |> Seq.where (fun [|a; b; c |] -> hypernet.Contains(System.String.Concat([|b;a;b|])))
    |> Seq.isEmpty
    |> not

let howManySupportTLS (ips: string[]) =
    ips
    |> Array.filter containsABBA''
    |> Array.length

let howManySupportSSL (ips: string[]) =
    ips
    |> Array.filter containsABA
    |> Array.length

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    // let testInput = File.ReadAllLines("sample.txt")

    input |> howManySupportTLS |> printfn "Part 1: %d"
    input |> howManySupportSSL |> printfn "Part 2: %A"

    0 // return an integer exit code
