let isSum lst =
    let n::l = lst
    let rec findSum l =
        match l with
        | [] -> false
        | h::t -> 
            match List.map (fun el -> el + h) t
                    |> List.where ((=)n)
                    |> List.tryHead with
            | Some _ -> true
            | None -> findSum t
    findSum l
    
let findWrongNumber window input =
    input 
    |> Seq.windowed (window + 1)
    |> Seq.map (Seq.rev >> Seq.toList)
    |> Seq.filter (isSum >> not)
    |> Seq.head
    |> Seq.head

let findWeakness target input =
    
    let rec isSum remainder l =
        match l with
        | [] -> None
        | h::t when h > remainder -> None
        | h::t when h = remainder -> Some [h]
        | h::t ->
            match isSum (remainder - h) t with
            | Some l -> Some <| h::l
            | None -> None
    
    let rec forEachSublist l =
        match l with
        | [] -> []
        | h::t -> isSum target l :: forEachSublist t
        
    match input
        |> forEachSublist
        |> List.where ((<>)None)
        |> List.head with
    | Some l -> List.min l + List.max l
    | None -> failwith "fail"
    
let parse = Seq.map int64 >> Seq.toList
    
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt") |> parse
    let target = input |> findWrongNumber 25
    target |> printfn "Part 1: %d"
    input |> findWeakness target |> printfn "Part 2: %d"
    
    
    0 // return an integer exit code