// Learn more about F# at http://fsharp.org

open System

let lookAndSay (n: char list) =
    List.fold (fun list c ->
        match list with
        | [] -> (c, 1)::[]
        | (last, count)::rest when last = c -> (last, count + 1)::rest
        | last::rest -> (c, 1)::last::rest) [] n
    |> List.map (fun (c, count) -> sprintf "%d%c" count c |> Seq.toList)
    |> List.rev
    |> List.concat
    
let repeat count f start =
    Seq.fold (fun i c ->
        let o = f i
//        printfn "%d %d" c (Seq.length o)
        o) (Seq.toList start) [1..count]
    
[<EntryPoint>]
let main argv =
//    repeat 5 lookAndSay "1" |> printfn "%A"
    
    repeat 40 lookAndSay "1321131112" |> Seq.length |> printfn "part1: length of output %A"
    repeat 50 lookAndSay "1321131112" |> Seq.length |> printfn "part2: length of output %A"
    
    0 // return an integer exit code
