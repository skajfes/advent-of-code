open System

type Ingredient = int64 * string
type Reaction = Ingredient list * Ingredient

let chemical ((_, chemical):Ingredient) = chemical
let result ((_, t): Reaction) = t
let ingredient (i:string) = 
    let [| quantity; chemical |] = i.Split(" ")
    Ingredient (int64 quantity, chemical)

let parse (input: string list) =
    input 
    |> List.map (fun i -> 
        let [| inputs; result |] = i.Split(" => ") 
        Reaction (inputs.Split(", ") |> Array.map ingredient |> Array.toList, ingredient result))

let findReaction c reactions =
    List.filter (result >> chemical >> (=)c) reactions 
    |> List.head

let rec totalOre (requiredQ, chemical) reactions extras =
    let extraQ ch = 
        match Map.tryFind ch extras with
        | Some q -> q
        | None -> 0L

    match chemical with
    | "ORE" -> (requiredQ, extras)
    | ic when extraQ chemical >= requiredQ -> 
        // found in extras
        (0L, Map.add ic (extras.[ic] - requiredQ) extras)
    | ic when extraQ chemical > 0L -> 
        // found some in extras
        let extras = Map.remove ic extras
        // produce the rest
        totalOre ((requiredQ - extraQ chemical), ic) reactions extras
    | _ -> 
        // produce
        let make multiplier (ore, extras) (q, c) =            
            let ingCost, newNewExtras = totalOre (multiplier * q, c) reactions extras
            (ore + ingCost, newNewExtras)

        let ingredients, (tq, _) = findReaction (chemical) reactions
        let multiplier = int64 <| ceil ((float requiredQ) / (float tq))
        let total, extras = List.fold (make multiplier) (0L, extras) ingredients

        // add leftover to extras
        let xtra = extraQ chemical
        let extras = 
            match tq * multiplier - requiredQ with
            | l when l > 0L -> Map.add chemical (l + xtra) extras
            | _ -> extras
            
        (total, extras)

let findMaxFuel ore reactions =
    let rec findMax fuel diff =
        let total = totalOre (fuel, "FUEL") reactions Map.empty |> fst
        if total > ore
        then findMax (fuel - diff) (diff / 2L)
        else if diff = 0L then fuel
        else findMax (fuel + diff) diff

    findMax 1L 1000000L

[<EntryPoint>]
let main argv =

    let reactions = IO.File.ReadAllLines("input.txt") |> Array.toList |> parse

    // part 1   
    totalOre (1L, "FUEL") reactions Map.empty |> fst
    |> ignore

    // part 2
    findMaxFuel 1000000000000L reactions
    |> ignore
    
    0 // return an integer exit code
