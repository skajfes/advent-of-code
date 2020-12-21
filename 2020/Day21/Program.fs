open System
open System.IO

let testInput = File.ReadAllLines("sample.txt")

let parse (input: string[]) = 
    input
    |> Array.toList
    |> List.map (fun l ->
        let splt = l.Split('(')
        let ing = splt.[0]
        let alerg = splt.[1]
        (ing.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set.ofArray,
         alerg.Replace(")", "").Replace("contains ", "").Replace(",", "").Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Set.ofArray))

type Ingredients =
    | Single of string
    | Multiple of Set<string>
    
let mapIngredients ings =
    
    let rec sortOutSingles found =
        let singles =
            found
            |> Map.filter (fun _ is ->
                match is with
                | Multiple is when Set.count is = 1 -> true
                | _ -> false)
            |> Map.toList
            |> List.map (fun (a, is) -> a, match is with | Multiple is -> Set.minElement is)
        
        if List.length singles > 0 then
            singles
            |> List.fold (fun found (a, i) ->
                Map.map (fun fa fi ->
                    if fa = a then Single i
                    else match fi with
                            | Single fi -> Single fi
                            | Multiple fi -> Multiple <| Set.remove i fi) found ) found
            |> sortOutSingles
        else found
         
    (Map.empty, ings)
    ||> List.fold (fun found (ing, alerg) ->
            Set.fold (fun found a ->
                let matches =
                    match Map.tryFind a found with
                    | Some (Multiple igs) -> Multiple <| Set.intersect igs ing
                    | Some (Single i) -> Single i
                    | None -> Multiple ing
                Map.add a matches found
                |> sortOutSingles ) found alerg)

let unmapped ings found =
    let all =
        List.map (fst>>Set.toList) ings
        |> List.concat
        
    found
    |> Map.fold (fun all _ ->
        function
        | Single i -> List.filter (fun a -> a <> i) all
        | _ -> failwith "all should be single") all
    |> List.length

let dangerList found =
    found
    |> Map.toList
    |> List.sort
    |> List.map (snd >> function | Single e -> e | _ -> failwith "not all single")
    |> String.concat ","
    
[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    
    let ings = input |> parse
    ings |> mapIngredients |> unmapped ings |> printfn "Part 1: %d"
    ings |> mapIngredients |> dangerList |> printfn "Part 2: %s"
    
    0 // return an integer exit code