open System.IO

let parse (input: string[]) =
    input
    |> Array.map(fun line -> line |> Seq.map (string >> int) |> Seq.toArray)

let atCoord r c map =
    if r >= 0 && r < Array.length map && c >= 0 && c < Array.length map.[r]
    then Some map.[r].[c]
    else None

let neighbours r c map =
    [
        atCoord (r-1) c map
        atCoord (r+1) c map
        atCoord r (c-1) map
        atCoord r (c+1) map
    ]
    |> List.filter (function | Some _ -> true | _ -> false)
    |> List.map (function | Some x -> x | _ -> failwith "fail")

let risk x = 1 + x

let part1 map =
    map
    |> Array.mapi(fun r row -> row |> Array.mapi (fun c height -> height, neighbours r c map))
    |> Array.concat
    |> Array.filter (fun (height, neig) -> height < List.min neig)
    |> Array.map (fst >> risk)
    |> Array.sum


// let rec countPool (map) r c value =
//     match value with
//     | -9 -> 0
//     | h when h > 0 -> h
//     | x when x <= 0 -> 1 + markNeigh r c map
//         // map.[r].[c] <- 1 + countPool map (r+1) c value
//         //                + countPool map (r-1) c value
//         //                + countPool map r (c+1) value
//         //                + countPool map r (c-1) value

let neighCoord r c =
    [ (r - 1, c)
      (r + 1, c)
      (r, c - 1)
      (r, c + 1) ]
    |> Set.ofList

let part2 map =
    map
    |> Array.mapi (fun r row -> row |> Array.mapi (fun c value -> (r,c,value)))
    |> Array.concat
    |> Array.filter (fun (r, c, v) -> v <> 9)
    |> Array.map (fun (r,c,v) -> r,c)
    |> Array.fold (fun pools (r,c) ->
            // printfn "%A" pools
            match pools |> List.filter (fun pool -> pool |> Set.intersect (neighCoord r c) |> Set.isEmpty |> not) with
            | [] -> (Set.empty |> Set.add (r,c)) :: pools
            | [pool] -> (pool |> Set.add (r,c)) :: (pools |> List.filter ((<>)pool))
            | [pool1; pool2] -> (pool1 |> Set.union pool2 |> Set.add (r,c)) :: (pools |> List.filter ((<>)pool1) |> List.filter ((<>)pool2))
            | _ -> failwith "fail"
        ) []
    |> List.map Set.count
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)


[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part2 |> printfn "%A"

    // input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
