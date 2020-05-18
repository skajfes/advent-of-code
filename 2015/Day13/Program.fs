open System
open System.Text.RegularExpressions

let parse input =
    input
    |> Seq.map (fun line ->
        let m = Regex.Match(line, @"^(\w+) .* (gain|lose) (\d+) .* (\w+).$")
        let g = m.Groups
        let points = (int (g.[3].Value)) * (if g.[2].Value = "gain" then 1 else -1)
        (g.[1].Value, g.[4].Value, points))
    |> Seq.toList
    
let people list =
    list
    |> List.map (fun (a, _, _) -> a)
    |> List.distinct
    
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let happiness rules people =
    let rule l r =
        rules
        |> List.filter (fun (a, b, _) -> a = l && b = r)
        |> List.tryHead
        |> function
           | Some (_, _, h) -> h
           | None -> 0
        
    List.indexed people
    |> List.fold (fun sum (i, p) ->
        let left = people.[(i - 1 + people.Length) % people.Length]
        let right = people.[(i + 1) % people.Length]
        sum + rule p left + rule p right) 0 
        
let mostHappiness withMe rules =
    rules
    |> people
    |> (fun x -> match withMe with
                 | true -> List.append ["me"] x
                 | false -> x)
    |> permute
    |> List.map (fun l -> happiness rules l)
    |> List.max

[<EntryPoint>]
let main argv =
    // test data
    """Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."""
    |> (fun x -> x.Split('\n'))
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> System.String.IsNullOrWhiteSpace(x) |> not)
    |> parse
    |> mostHappiness false
    |> printfn "%A"
    
    // input
    let input = IO.File.ReadAllLines("input.txt") |> parse
    
    input
    |> mostHappiness false
    |> printfn "part1: %A"
    
    input
    |> mostHappiness true
    |> printfn "part2: %A"
    
    
    
    0 // return an integer exit code
