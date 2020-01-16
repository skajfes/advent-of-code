// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =

    let lower = 347312
    let upper = 805915

    let test n = 
        let digits = 
            List.unfold (function
                         | 0 -> None
                         | r -> Some (r%10, r / 10)) n
        let increasingNumbers =
            List.fold (fun s n -> 
                match s with
                | (false, _) -> (false, 0)
                | (true, max) when n > max -> (false, 0)
                | (true, _) -> (true, n)
            ) (true, List.head digits) digits
            |> fst
        
        let adjacentSame =
            List.groupBy id digits
            |> List.tryFind (fun (_, l) -> List.length l >= 2)
            |> function
                | None -> false
                | _ -> true
           
        increasingNumbers && adjacentSame

    let numbers = 
        seq [lower..upper]
        |> Seq.filter test
     

    numbers |> Seq.length

