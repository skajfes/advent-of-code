
let nextCode code = code * 252533L % 33554393L

let codeAt row col =
    let r1 =
        [1..col]
        |> List.fold (fun c x -> c + x) 0
    
    [2..row]
    |> List.fold (fun c x -> c + x + col-2) r1
    
[<EntryPoint>]
let main argv =
    let c = codeAt 3010 3019
    
    [2..c] |> Seq.fold (fun c x -> nextCode c) 20151125L |> printfn "%d"
    0 // return an integer exit code
    