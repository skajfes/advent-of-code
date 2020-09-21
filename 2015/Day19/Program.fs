open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open FSharpx.Collections

let rec tokenize input =
    match input with
    | "" -> []
    | i when i.Length = 1 -> [i]
    | i when Char.IsUpper (i.[0]) && Char.IsLower(i.[1]) ->
        i.Substring(0, 2) :: tokenize (input.Substring(2))
    | i -> i.Substring(0, 1) :: tokenize (input.Substring(1))

let flatten tokens =
    List.fold (fun s t -> s + t) "" tokens
    
let parseRules input =
    let r = Regex(@"^(\w+) =\> (\w+)$")
    
    input
    |> List.map (fun l ->
        let m = r.Match(l)
        m.Groups.[1].Value, m.Groups.[2].Value)
    |> List.map (fun (s, d) -> s, tokenize d)
    
let parse (input: string) =
    let target::rules = input.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Seq.rev |> Seq.toList
    target, parseRules rules
    

let rec transform rules inputTokens =
    let applyRules rules inputTokens =
        match inputTokens with
        | [] -> []
        | h::t -> 
            rules
            |> List.filter (fun (s, _) -> h = s)
            |> List.map (fun (_, d) -> List.append d t)
            
    match inputTokens with
    | [] -> []
    | h::t ->
        applyRules rules inputTokens
        |> List.append (transform rules t |> List.map (fun t -> h::t))
        |> List.distinct
        
let transformAll rules input =
    tokenize input
    |> transform rules
    |> List.length
    
let trim c s =
    if String.length s > c then s.Substring(0, c) + "..." else s
    
let rec truncate target input =
    match target, input with
    | t::ts, i::is when i = t -> truncate ts is
    | _, _ -> input, target
    
let inv f a b = f b a
    
let calculateScore step rules target current =
    let rec samePrefix current target =
        match current, target with
        | c::cs, t::ts when c = t -> 1 + (samePrefix cs ts)
        | _ -> 0
        
    let lc = List.length current
    let lt = List.length target
    let prefix = samePrefix current target
    let length = if lc > lt then Int32.MaxValue else lc
    let unguessedLength = if length - prefix < 10 then 0 else length - prefix
    let score = 30*(lt - prefix) + length + 100 * unguessedLength
    
    // log
    printfn "%d\t%d\t%d\t%d\t%s"
        score step lc prefix (current |> List.skip prefix |> flatten |> trim 100) 
        
    (score, step + 1, current)
    
    
let transformFromE rules target =
    
    let rec find (visited: HashSet<string list>) queue =
        let (score, step, current), queue = PriorityQueue.pop queue
        
        // answer was too high
        if step >= 208 || visited.Contains(current)
        then find visited queue
        else
        
        visited.Add(current) |> ignore
        
        if (target = current) then step else
            
        let queue =
            transform rules current // all neighbours
            |> List.filter (fun s -> visited.Contains(s) |> not)
            |> List.map (calculateScore step rules target)
            |> List.fold (inv PriorityQueue.insert) queue
        
        find visited queue
        
    PriorityQueue.empty false
    |> PriorityQueue.insert (0, 0, tokenize "e")
    |> find (HashSet<string list>()) 
    
[<EntryPoint>]
let main argv =
    let input, rules = IO.File.ReadAllText("input.txt") |> parse
    
    transformAll rules input |> printfn "part1: %A"
    transformFromE rules (tokenize input) |> printfn "part2: %A"
    // https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/
    // t - number of tokens
    // p - number of Ar and Rn tokens
    // c - number of Y tokens
    // t - p - 2*c - 1 = 207
    
    0 

