open System.ComponentModel.DataAnnotations
open System.Text.RegularExpressions
let testInput =
    """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"""

let testInput2 =
    """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"""

let parseTicket (ticket: string) =
    ticket.Split(',')
    |> Array.map int
    
let parseRules (rules: string) =
    rules.Split("\r\n")
    |> Array.map (fun r ->
        let m = Regex.Match(r, @"^(.+): (\d+)-(\d+) or (\d+)-(\d+)$")
        let g (n: int) = m.Groups.[n].Value |> int
        if m.Success
        then m.Groups.[1].Value, (g 2, g 3), (g 4, g 5)
        else failwithf "rule not matched '%s'" r )
    
    
    
let parse (input:string) =
    let [| rules; your_ticket; nearby_tickets; |] = input.Split("\r\n\r\n")
    
    parseRules rules, parseTicket (your_ticket.Split("\r\n") |> Array.last), Array.map parseTicket (nearby_tickets.Split("\n") |> Array.tail)
    


let invalidField rules ticket =
    ticket
    |> Array.tryFind (fun field ->
        Array.exists (fun (_, (a, b), (c, d)) ->
            field >= a && field <= b || field >= c && field <= d ) rules |> not)
    

let validRule field (_, (a, b), (c, d)) = field >= a && field <= b || field >= c && field <= d 
let valid rules ticket =
    ticket
    |> Array.forall (fun field ->
        Array.exists (validRule field) rules )
            
let invalidTickets rules tickets =
        
    tickets
    |> Array.map (invalidField rules)
    |> Array.filter ((<>) None)
    |> Array.map (function | Some x -> x | None -> 0)
    |> Array.sum
    
let determinePositions rules tickets =
    let validTickets = Array.filter (valid rules) tickets
    
    let fields =
        validTickets
        |> Array.transpose
        |> Array.mapi (fun i a -> i+1, a)
        
    let ruleName (name, _, _) = name
    
    let rulePositions =
        rules
        |> Array.map (fun rule ->
            ruleName rule,
            fields
            |> Array.filter (fun (_, fields) ->
                fields |> Array.forall (fun field -> validRule field rule))
            |> Array.map fst
            |> Set.ofArray
            )
        |> Array.sortBy (fun (_, s) -> Set.count s)
    
    rulePositions
    |> Array.fold (fun (res, taken_positions) (rule, possible_positions) ->
        let positions = Set.difference possible_positions taken_positions
        if Set.count positions <> 1 then failwith "not good" else
        let pos = Set.toArray positions |> Array.head
            
        ((rule, pos)::res, Set.add pos taken_positions)) ([], Set.empty)
    |> fst
    
let myTicketValue ticket positions =
    positions
    |> List.filter (fun (rule: string, _) -> rule.StartsWith("departure"))
    |> List.map (snd)
    |> List.map (fun p -> Array.item (p - 1) ticket |> int64)
    |> List.fold (fun m v -> m * v) 1L
    
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllText("input.txt")
    let (rules, ticket, other_tickets) = parse input
    invalidTickets rules other_tickets |> printfn "Part 1: %d"
    determinePositions rules other_tickets |> myTicketValue ticket |> printfn "Part 2: %d"
    
    0 // return an integer exit code