
let parse (input: string) =
    input.Split("\r\n")
    |> Seq.fold (fun (buf, res) l ->
        match l with
        | "" -> [], buf :: res
        | _ -> l :: buf, res
        ) ([], [])
    |> (fun (buf, res) -> buf :: res)

let sumUniqueAnswers =
    List.map Seq.concat
    >> List.map (Seq.distinct >> Seq.length)
    >> List.sum
    
let sumSameAnswers =
    List.where ((<>)List.empty)
    >> List.map (List.map Set.ofSeq
                 >> Set.intersectMany
                 >> Set.count)
    >> List.sum
    
[<EntryPoint>]
let main argv =
    let answers = System.IO.File.ReadAllText("input.txt") |> parse
    answers |> sumUniqueAnswers |> printfn "%A"
    answers |> sumSameAnswers |> printfn "%A"
    0 // return an integer exit code