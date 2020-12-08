open System.Text.RegularExpressions

type Bag = Bag of string
type Rule = Bag * ((int * Bag) list)

let parse input =
    input
    |> List.map (fun r ->
        let matches = Regex.Match(r, @"(.*) bags contain(?: (\d+) (.*?) bags?[,\.])+")
        if matches.Success then
            let bag = Bag matches.Groups.[1].Value 
            let rules = 
                [0..(Seq.length matches.Groups.[2].Captures)-1]
                |> List.map (fun i -> (int matches.Groups.[2].Captures.[i].Value, Bag matches.Groups.[3].Captures.[i].Value))
            
            (bag, rules)
        else
            let noMatch = Regex.Match(r, @"(.*) bags contain no other bags")
            (Bag noMatch.Groups.[1].Value, []) )
    
let howManyCarryGold rules =
    let rec bagsContaining targetBag =
        let bags =
            rules
            |> List.filter (fun (bag, bags) ->
                List.exists (fun (q, b) -> b = targetBag) bags)
            |> List.map fst
            |> Set.ofList
        Set.fold (fun res b -> bagsContaining b |> Set.union res) bags bags
        
    bagsContaining (Bag "shiny gold")
    |> Set.count
    
let howManyInsideGold rules =
    let rec countBags bag =
        let rule =
            List.filter (fst >> (=)bag) rules
            |> List.head
        match snd rule with
        | [] -> 0
        | rs -> List.sumBy (fun (c, b) -> c * (1 + countBags b)) rs
        
    countBags (Bag "shiny gold")
    
    
[<EntryPoint>]
let main args =
    let input = System.IO.File.ReadAllLines("input.txt") |> Seq.toList
    input |> parse |> howManyCarryGold |> printfn "Part 1: %A"
    input |> parse |> howManyInsideGold |> printfn "Part 2: %A"
    
    0 // return an integer exit code