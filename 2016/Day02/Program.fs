let normalKeypad start direction =
    match direction, start with
    | 'R', "1" -> "2"
    | 'R', "2" -> "3"
    | 'R', "4" -> "5"
    | 'R', "5" -> "6"
    | 'R', "7" -> "8"
    | 'R', "8" -> "9"
    | 'L', "2" -> "1"
    | 'L', "3" -> "2"
    | 'L', "5" -> "4"
    | 'L', "6" -> "5"
    | 'L', "8" -> "7"
    | 'L', "9" -> "8"
    | 'U', "4" -> "1"
    | 'U', "5" -> "2"
    | 'U', "6" -> "3"
    | 'U', "7" -> "4"
    | 'U', "8" -> "5"
    | 'U', "9" -> "6"
    | 'D', "1" -> "4"
    | 'D', "2" -> "5"
    | 'D', "3" -> "6"
    | 'D', "4" -> "7"
    | 'D', "5" -> "8"
    | 'D', "6" -> "9"
    | _ -> start
    
let superKeypad start direction =
    match direction, start with
    | 'R', "2" -> "3"
    | 'R', "3" -> "4"
    | 'R', "5" -> "6"
    | 'R', "6" -> "7"
    | 'R', "7" -> "8"
    | 'R', "8" -> "9"
    | 'R', "A" -> "B"
    | 'R', "B" -> "C"
    | 'L', "3" -> "2"
    | 'L', "4" -> "3"
    | 'L', "6" -> "5"
    | 'L', "7" -> "6"
    | 'L', "8" -> "7"
    | 'L', "9" -> "8"
    | 'L', "B" -> "A"
    | 'L', "C" -> "B"
    | 'U', "3" -> "1"
    | 'U', "6" -> "2"
    | 'U', "7" -> "3"
    | 'U', "8" -> "4"
    | 'U', "A" -> "6"
    | 'U', "B" -> "7"
    | 'U', "C" -> "8"
    | 'U', "D" -> "B"
    | 'D', "1" -> "3"
    | 'D', "2" -> "6"
    | 'D', "3" -> "7"
    | 'D', "4" -> "8"
    | 'D', "6" -> "A"
    | 'D', "7" -> "B"
    | 'D', "8" -> "C"
    | 'D', "B" -> "D"
    | _ -> start
    
    
let rec getCode keypad start directions =
    match directions with
    | [] -> start
    | d::dirs ->
        let nextDigit = keypad start d
        getCode keypad nextDigit dirs
    
let toKeys keypad input =
   input
   |> Seq.mapFold (fun lastCode directions ->
       let code = directions
                  |> Seq.toList
                  |> getCode keypad lastCode 
       code, code) "5"
   |> fun (code, _) -> code |> System.String.Concat

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    input |> toKeys normalKeypad |> printfn "Part 1: %s"
    input |> toKeys superKeypad |> printfn "Part 2: %s"
    0 // return an integer exit code