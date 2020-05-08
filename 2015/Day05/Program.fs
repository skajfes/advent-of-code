// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions

type NaughtyOrNice = Naughty | Nice

let naughtyOrNice (s: string) =
    let vowelCount = Seq.filter (fun c -> Seq.contains c "aeiou") s |> Seq.length
    if vowelCount < 3 then Naughty else
        
    let hasDoubles = Seq.pairwise s |> Seq.exists (fun (a, b) -> a = b)
    if hasDoubles = false then Naughty else
        
    let hasForbiddenStrings = Seq.map (fun t -> s.Contains((string)t)) ["ab"; "cd"; "pq"; "xy"] |> Seq.exists ((=)true) 
    if hasForbiddenStrings then Naughty else Nice
    
let naughtyOrTwice (s: string) =
    let hasPair = Regex.IsMatch(s, "(..).*\1")
    let hasRepeatingLetter = Regex.IsMatch(s, "(.).\1")
    if hasPair && hasRepeatingLetter then Nice else Naughty

let howManyNice f input =
    input
    |> Seq.map f
    |> Seq.filter ((=) Nice)
    |> Seq.length
    
[<EntryPoint>]
let main argv =
    // tests
    //    naughtyOrNice "ugknbfddgicrmopn" |> printfn "ugknbfddgicrmopn %A"
    //    naughtyOrNice "aaa" |> printfn "aaa %A"
    //    naughtyOrNice "jchzalrnumimnmhp" |> printfn "jchzalrnumimnmhp %A"
    //    naughtyOrNice "haegwjzuvuyypxyu" |> printfn "haegwjzuvuyypxyu %A"
    //    naughtyOrNice "dvszwmarrgswjxmb" |> printfn "dvszwmarrgswjxmb %A"
    
    naughtyOrTwice "qjhvhtzxzqqjkmpb" |> printfn "qjhvhtzxzqqjkmpb %A"
    naughtyOrTwice "xxyxx"|> printfn "xxyxx %A"
    naughtyOrTwice "uurcxstgmygtbstg"|> printfn "uurcxstgmygtbstg %A"
    naughtyOrTwice "ieodomkazucvgmuy"|> printfn "ieodomkazucvgmuy %A"
    
    let input = IO.File.ReadAllLines("input.txt")
    printfn "Part1: nice strings %d" (howManyNice naughtyOrNice input)
    printfn "Part2: nice strings %d" (howManyNice naughtyOrTwice input)
    
    0 // return an integer exit code
