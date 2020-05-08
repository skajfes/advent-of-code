// Learn more about F# at http://fsharp.org

open System
open System.Text

let toString (bytes: byte[]) =
    Array.fold (fun a b -> a + sprintf "%02x" b) "" bytes
    
let md5 = System.Security.Cryptography.MD5.Create()
let hash (input: string) =
    input
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> toString
    
let find input precision =
    let target = String('0', precision)
    let rec findHash input n =
        match sprintf "%s%d" input n |> hash with
        | h when h.StartsWith(target) -> n
        | _ -> findHash input (n + 1)
        
    findHash input 0
    
    

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    
    //    find "abcdef" |> printfn "hash for abcdef should equal 609043 %d"
    //    find "pqrstuv" |> printfn "hash for pqrstuv should equal 1048970 %d"
    
    find "iwrupvqb" 5 |> printfn "part1: %d"
    find "iwrupvqb" 6 |> printfn "part2: %d"
    
    0 // return an integer exit code
