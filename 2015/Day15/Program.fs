open System
open System.Text.RegularExpressions

let test =
    [ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
      "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3" ]

type Ingredient = {
    Name: string
    Capacity: int
    Durability: int
    Flavor: int
    Texture: int
    Calories: int
}

let parse input =
    let parseLine line =
        let m = Regex.Match(line, @"(\w+): capacity (\-?\d+), durability (\-?\d+), flavor (\-?\d+), texture (\-?\d+), calories (\-?\d+)")
        let s (id:int) = m.Groups.[id].Value
        let i = s >> int
            
        { Name = s 1; Capacity = i 2; Durability = i 3; Flavor = i 4; Texture = i 5; Calories = i 6 }
    
    input
    |> Seq.map parseLine
    |> Seq.toList
    
let scoreRecipe capCalories ingredients =
    let sum f =
        ingredients
        |> List.map (fun (ingredient, quantity) -> (f ingredient) * (quantity)) 
        |> List.sum
        |> function
            | s when s > 0 -> s
            | _ -> 0
    
    let calories = sum (fun i -> i.Calories)
    if capCalories && calories <> 500 then 0 else
        
    let capacity = sum (fun i -> i.Capacity)
    let durability = sum (fun i -> i.Durability)
    let flavor = sum (fun i -> i.Flavor)
    let texture = sum (fun i -> i.Texture)
    
    capacity * durability * flavor * texture

let generateRecipes ingredients =
    
    [for a in [1..100] do
         for b in [1..100] do
             for c in [1..100] -> [a; b; c; 100-a-b-c]]
    //[for a in [1..100] -> [a; 100-a]]
    |> List.filter (List.forall ((<)0))
    |> List.filter (List.sum >> ((=)100))
    |> List.map (List.zip ingredients)
    
let maxScore scoring ingredients =
    generateRecipes ingredients
    |> List.map (fun recipe -> (List.map (fun (r, d) -> r.Name, d) recipe, scoring recipe))
    |> List.maxBy snd
   
[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllLines("input.txt") |> parse
    
    maxScore (scoreRecipe false) input |> printfn "part 1: %A"
    maxScore (scoreRecipe true) input |> printfn "part 2: %A"
    
    0 // return an integer exit code
