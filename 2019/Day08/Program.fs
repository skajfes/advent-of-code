open System

let rec split len input =
    if Seq.isEmpty input then []
    else
        let row = Seq.take len input |> Seq.toList
        let rest = Seq.skip len input
        row::split len rest

let layers width height input =
    let res = width * height
    input 
    |> Seq.map (string >> int)
    |> split res

let signature layers =
    let count d = List.filter ((=)d) >> List.length
    List.minBy (count 0) layers
    |> (fun l -> 
            let ones = count 1 l
            let twos = count 2 l
            ones * twos)

let flatten layers =
    layers
    |> List.transpose
    |> List.map (fun l -> List.filter ((<>)2) l |> List.head)

let render width layer =
    let charMap = 
        function
        | 0 -> " "
        | 1 -> "#"
        | _ -> failwith "invalid char"
    split width layer
    |> List.iter (List.map charMap >> String.concat "" >> printfn "%s")


[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllText("input.txt")

    // part 1
    input 
    |> layers 25 6
    |> signature
    |> ignore

    // part 2
    input
    |> layers 25 6
    |> flatten
    |> render 25
    |> ignore

    0