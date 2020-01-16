// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    
    let parse (input: string) =
        input.Split(',')
        |> Seq.map (fun x -> (x.[0], int x.[1..]))
        |> Seq.toList
    
    let move wire =
        let coord (x, y) (d, l) =
            match d with
            | 'R' -> (seq [for i in 1..l -> (x + i, y)], (x + l, y))
            | 'L' -> (seq [for i in 1..l -> (x - i, y)], (x - l, y))
            | 'U' -> (seq [for i in 1..l -> (x, y + i)], (x, y + l))
            | 'D' -> (seq [for i in 1..l -> (x, y - i)], (x, y - l))
            | _ -> failwith "unknown direction"

        wire
        |> Seq.mapFold coord (0, 0) 
        |> fst
        |> Seq.concat
    
    let lines = IO.File.ReadAllLines("input.txt")
    //let lines = ["R75,D30,R83,U83,L12,D49,R71,U7,L72";"U62,R66,U55,R34,D71,R55,D58,R83"]
    let wireA = (parse >> move) lines.[0]
    let wireB = (parse >> move) lines.[1]

    let intersections =
        Set.intersect (Set.ofSeq wireA) (Set.ofSeq wireB) 

    // first part
    // let closest =
    //     intersections
    //     |> Seq.map (fun (x, y) -> Math.Abs x + Math.Abs y)
    //     |> Seq.min

    // second part

    let toLength wire = 
        Set.map (fun x -> (x, 1 + Seq.findIndex ((=)x) wire) ) intersections
        |> Set.toSeq
        |> Seq.sort

    Seq.zip (toLength wireA) (toLength wireB)
    |> Seq.map (fun (a, b) -> snd a + snd b) 
    |> Seq.min

    