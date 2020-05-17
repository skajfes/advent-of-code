let increment pass =
    Seq.foldBack (fun c (res, overflow) ->
        match c + overflow with
        | c when c > 'z' -> ("a" + res, char 1)
        | c -> (string c + res, char 0)
        ) pass ("", char 1) |> fst
 
let letterSequence pass =
    Seq.windowed 3 pass
    |> Seq.exists (fun [| a; b; c |] -> int b - int a = 1 && int c - int a = 2)
    
let forbiddenLetters pass =
    "iol"
    |> Seq.exists (fun c -> Seq.contains c pass)
    |> not
    
let twoPairs pass =
    Seq.windowed 2 pass
    |> Seq.filter (fun [| a; b |] -> a = b)
    |> Seq.map (Array.head)
    |> Seq.distinct
    |> Seq.length >= 2

let isPasswordValid pass =
    let f = forbiddenLetters pass
    (f && twoPairs pass && letterSequence pass, f)
    
let avoidForbidden (pass: seq<char>) =
    Seq.fold (fun (pass, reset) c ->
        match string c |> forbiddenLetters, reset with
        | _, true -> pass + "a", true
        | false, _ -> pass + (string c |> increment), true
        | true, _ -> pass + (string c), false) ("", false) pass
    |> fst
        
let rec nextPassword pass =
    //printfn "%s %b %b %b" pass (forbiddenLetters pass) (twoPairs pass) (letterSequence pass)
    let pass = increment pass
    match isPasswordValid pass with
    | true, _ -> pass
    | false, false -> avoidForbidden pass |> nextPassword 
    | false, true -> pass |> nextPassword

[<EntryPoint>]
let main argv =
    //nextPassword "abcdefgh" |> printfn "abcdefgh %s" 
    //nextPassword "ghijklmn" |> printfn "ghijklmn %s" 
    
    nextPassword "hxbxwxba" |> printfn "part1: %s"
    nextPassword "hxbxwxba" |> nextPassword |> printfn "part2: %s"
    
    0 // return an integer exit code
