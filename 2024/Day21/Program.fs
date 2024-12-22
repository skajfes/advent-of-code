open System.IO

let parse (input: string[]) = 
    input
    |> Array.map seq

let to_pad =
    Array.mapi (fun i row -> row |> Array.mapi (fun j c -> c, (i, j)))
    >> Array.concat
    >> Map.ofArray

let numpad =
    [|
        [|'7'; '8'; '9'|]
        [|'4'; '5'; '6'|]
        [|'1'; '2'; '3'|]
        [|' '; '0'; 'A'|]
    |]
    |> to_pad

let arrowpad =
    [|
        [|' '; '^'; 'A'|]
        [|'<'; 'v'; '>'|]
    |]
    |> to_pad

let from_to pad a b =
    let ar, ac = Map.find a pad
    let br, bc = Map.find b pad
    let v_diff, v_dir = if ar > br then -1, '^' else 1, 'v'
    let h_diff, h_dir = if ac > bc then -1, '<' else 1, '>'
    let path =
        [for _ in ar..v_diff..br-v_diff do v_dir ]
        |> List.append [for _ in ac..h_diff..bc-h_diff do h_dir ]
        |> List.append ['A']
    // path
    let over_hole =
        (path, (ar, ac))
        ||> List.mapFoldBack (fun el (r, c) ->
                match el with
                | '^' -> (r-1, c), (r-1, c)
                | 'v' -> (r+1, c), (r+1, c)
                | '<' -> (r, c-1), (r, c-1)
                | '>' -> (r, c+1), (r, c+1)
                | _ -> (r, c), (r, c)
            )
        |> fst
        |> List.map (fun c -> pad |> Map.pick (fun k v -> if v = c then Some k else None) )
        |> List.exists (function | ' ' -> true | _ -> false)

    if over_hole
    then
        [for _ in ac..h_diff..bc-h_diff do h_dir ]
        |> List.append [for _ in ar..v_diff..br-v_diff do v_dir ]
        |> List.append ['A']
    else path


let decode pad code =
    code
    |> Seq.fold (fun (pos, commands) c ->
        let cs = from_to pad pos c
        c, cs::commands
    ) ('A', [])
    |> fun (_, commands) -> commands |> Seq.rev |> Seq.concat

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let permute list =
    let rec permute' = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute' xs)

    permute' list
    |> List.distinct

let over_hole pad path (ar, ac) =
    ( (ar, ac), path)
    ||> List.mapFold (fun (r, c) el ->
            match el with
            | '^' -> (r-1, c), (r-1, c)
            | 'v' -> (r+1, c), (r+1, c)
            | '<' -> (r, c-1), (r, c-1)
            | '>' -> (r, c+1), (r, c+1)
            | _ -> (r, c), (r, c)
        )
    |> fst
    |> List.map (fun c -> pad |> Map.pick (fun k v -> if v = c then Some k else None) )
    |> List.exists (function | ' ' -> true | _ -> false)

let from_to' pad a b =
    let ar, ac = Map.find a pad
    let br, bc = Map.find b pad
    let v_diff, v_dir = if ar > br then -1, '^' else 1, 'v'
    let h_diff, h_dir = if ac > bc then -1, '<' else 1, '>'
    let path =
        [for _ in ar..v_diff..br-v_diff do v_dir ]
        |> List.append [for _ in ac..h_diff..bc-h_diff do h_dir ]

    path
    |> permute
    |> List.filter (fun p -> not (over_hole pad p (ar, ac)))
    |> List.map (fun a -> List.append a ['A'])

let decode' pad code =
    code
    |> Seq.fold (fun (pos, commands) c ->
        let cs = from_to' pad pos c
        c, cs::commands
    ) ('A', [])
    |> fun (_, commands) -> Seq.rev commands

let part1 input =
    input
    |> Array.map (decode' numpad)
    |> Array.map (Seq.map (List.map (decode' arrowpad)))
    |> Array.map (Seq.map (List.map (Seq.map (List.map (decode' arrowpad)))))
    |> Array.map (Seq.map (List.map (Seq.map (List.map (Seq.map (List.map List.length >> List.min))))))
    |> Array.map (Seq.map (List.map (Seq.map (List.map (Seq.sum)))))
    |> Array.map (Seq.map (List.map (Seq.map (List.min ))))
    |> Array.map (Seq.map (List.map (Seq.sum )))
    |> Array.map (Seq.map (List.min ))
    |> Array.map (Seq.sum )
    |> Array.mapi (fun i s -> s * (input[i] |> Seq.map string |> String.concat "" |> fun x -> x.Replace("A", "") |> int))
    |> Array.sum

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    // input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
