open System
open System.Text.RegularExpressions

let count (s: string) =
    let s' = Regex.Replace(s, @"^\""(.*)\""$", @"$1")
    let s'' = Regex.Replace(s', @"\\x[0-9a-f]{2}", "z")
    let s''' = Regex.Replace(s'', @"(\\""|\\\\)", "_")
    s.Length - s'''.Length
    
let countEncoded (s: string) =
    let s' = s.Replace("\"", "__")
    let s'' = s'.Replace("\\", "__")
    let s''' = sprintf @"""%s""" s''
    s'''.Length - s.Length
    
        
    
[<EntryPoint>]
let main argv =
    
//    [@""""""; @"""abc"""; @"""aaa\""aaa"""; @"""\x27"""]
//    |> Seq.map countEncoded
//    |> Seq.sum
//    |> printfn "%A"
    
    let input = IO.File.ReadAllLines("input.txt")
    
    input
    |> Seq.map count
    |> Seq.sum
    |> printfn "part1 %d"
    
    input
    |> Seq.map countEncoded
    |> Seq.sum
    |> printfn "part2 %d"
    
    0 // return an integer exit code
