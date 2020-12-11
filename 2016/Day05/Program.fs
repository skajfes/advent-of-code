
open System.Security.Cryptography
open System.Text

let md5 = MD5.Create()
let hash (input: string) =
    // only take the relevant 4 bytes of the hash (we're only interested in first 7 characters of the digest)
    let h = md5.ComputeHash(Encoding.UTF8.GetBytes(input)) |> Array.take 4
    match h with
    | [| 0uy; 0uy; a; b |] when a < 16uy ->
        // return the 6th and 7th digit directly
        Some (a.ToString("x"), (b/16uy).ToString("x"))
    | _ -> None

let findPassword (door: string) =
    let mutable password = ""
    Seq.unfold (fun c -> Some ( hash (door + (string c)), c + 1)) 1
    |> Seq.filter ((<>) None)
    |> Seq.map (function
        | Some (a, _) -> 
            password <- password + a
            System.Console.SetCursorPosition(0, 0)
            printfn "%s" password
            a
        | _ -> failwith "wrong")
    |> Seq.take 8
    |> System.String.Concat
    
let findBetterPassword' (door: string) =
    let mutable password = "        " |> Seq.toArray
    let rec generate c =
        let h = hash (door + (string c))
        match h with
        | Some (a, b) when a < "8" ->
            let idx = int a
            if password.[idx] = ' ' then
               password.[idx] <- b.[0]
               System.Console.SetCursorPosition(0, 2)
               printfn "%s" (System.String(password))
            if Seq.contains ' ' password |> not
            then System.String(password)
            else generate (c + 1)
        | _ -> generate (c + 1)
        
    generate 1 
    

[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    
    "abbhdwsy" |> findPassword |> printfn "Part 1: %s"
    
    let p1 = sw.ElapsedMilliseconds
    sw.Restart() |> ignore
    
    "abbhdwsy" |> findBetterPassword' |> printfn "Part 2: %s"
    
    let p2 = sw.ElapsedMilliseconds
    sw.Stop() |> ignore
    
    printfn "time 1: %d time 2: %d" p1 p2
    0 // return an integer exit code