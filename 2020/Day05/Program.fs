let convert input =
    let input =
        input
        |> Seq.map (function
            | 'F' | 'L' -> 0
            | 'B' | 'R' -> 1
            | _ -> failwith "invalid input")
    let rowString = Seq.take 7 input |> System.String.Concat
    let seatString = Seq.skip 7 input |> System.String.Concat
    System.Convert.ToInt32(rowString, 2), System.Convert.ToInt32(seatString, 2)

let seatId (r, c) = r * 8 + c

let highestSeatId inputs =
    inputs
    |> Seq.map (convert >> seatId) 
    |> Seq.max
    
let missingSeatId inputs =
    inputs
    |> Seq.map (convert >> seatId)
    |> Seq.sort
    |> Seq.windowed 2
    |> Seq.map (fun [| a; b |] -> a, b - a) // difference with next seat
    |> Seq.where (fun (a, b) -> b <> 1) // seats not sequential
    |> Seq.head 
    |> (fst >> (+) 1) // seat number for empty seat
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    
    highestSeatId input |> printfn "Part 1: %A"
    missingSeatId input |> printfn "Part 2: %A"
    
    0 // return an integer exit code