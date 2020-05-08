// Learn more about F# at http://fsharp.org

open System

let getPath input =
    let path =
        Seq.mapFold (fun (x, y) dir ->
            let h =
                match dir with
                | '<' -> (x - 1, y)
                | '>' -> (x + 1, y)
                | '^' -> (x, y - 1)
                | 'v' -> (x, y + 1)
                | _ -> failwith "invalid command"
            (h, h)) (0, 0) input

    (0, 0) :: ( fst path |> Seq.toList)
    
let distinctHouses path =
    path
    |> List.distinct
    |> List.length
    
let splitInput input =
    let indexed = Seq.indexed input
    
    let santaPath =
        indexed
        |> Seq.filter (fun (i, _) -> i % 2 = 0)
        |> Seq.map snd
    
    let robotPath =
        indexed
        |> Seq.filter (fun (i, _) -> i % 2 = 1)
        |> Seq.map snd
    
    (santaPath, robotPath)
    
let joinPath (santa, robot) =
    List.append (getPath santa) (getPath robot)
    

[<EntryPoint>]
let main argv =

    let input = IO.File.ReadAllText("input.txt")

    input |> getPath |> distinctHouses |> printfn "Part1: number of houses visited: %A"
    input |> splitInput |> joinPath |> distinctHouses |> printfn "Part2: number of houses with robot: %A"
    

//    houses "^v^v^v^v^v" |> printfn "%A"
    0 // return an integer exit code
