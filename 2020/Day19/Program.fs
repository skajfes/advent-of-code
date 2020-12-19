open System.Text.RegularExpressions

type RuleId = int
type Prods =
    | Rule of RuleId list
    | OrRule of RuleId list * RuleId list
    | Literal of string
type Rule = RuleId * Prods
    
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then
        Some [for g in Seq.tail <| m.Groups do [for c in g.Captures -> c.Value]]
    else None
    
let parseRules (lines: seq<string>) =
    lines
    |> Seq.map (function
        | Regex @"^(\d+):(?: (\d+))+$" [[id]; rules] -> int id, Rule (List.map int rules)
        | Regex @"^(\d+):(?: (\d+))+ \|(?: (\d+))+$" [[id]; rules_a; rules_b] -> int id, OrRule (List.map int rules_a, List.map int rules_b)
        | Regex @"^(\d+): ""(\w)""$" [[id]; [str]] -> int id, Literal (string str)
        | c -> failwithf "unknown %s" c
        )
    |> Seq.toList

let parse (input: seq<string>) = 
    let rules = Seq.takeWhile ((<>)"") input
    let input = Seq.skip (1 + Seq.length rules) input |> Seq.toList
    (parseRules rules, input)

let satisfyRule0 (rules: Rule list) input =
    let rec check ruleIds (input: string) =
        match ruleIds with
        | [] when String.length input = 0 -> true
        | _ when String.length input = 0 -> false
        | [] -> false
        | id::ruleIds ->
            match List.find (fst >> (=) id) rules |> snd with
            | Literal c when c = string input.[0] -> check ruleIds (input.Substring(1))
            | Literal _ -> false
            | Rule ps -> check (ps@ruleIds) input
            | OrRule (ps1, ps2) ->
                check (ps1@ruleIds) input || check (ps2@ruleIds) input
                
    check [0] input
    
let validEntries rules inputs =
    inputs
    |> List.filter (satisfyRule0 rules)
    |> List.length
    
let replaceRules rules inputs =
    let alteredRules = parseRules <| "8: 42 | 42 8\n11: 42 31 | 42 11 31".Split("\n")

    let rules = rules |> List.map (fun r ->
            match List.tryFind (fst >> (=) (fst r)) alteredRules with
            | Some rep -> rep
            | None -> r)
        
    rules, inputs
    

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")

    input |> parse ||> validEntries |> printfn "Part 1: %d"
    input |> parse ||> replaceRules ||> validEntries |> printfn "Part 2: %d"

    0 // return an integer exit code