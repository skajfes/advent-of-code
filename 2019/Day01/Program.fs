// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =
    let moduleFuel x =
        (decimal x) / 3.0m
        |> Math.Floor
        |> int
        |> (fun x -> x - 2)

    let fuelFuel x =
        List.unfold (fun t -> match moduleFuel t with
                              | m when m <= 0 -> None
                              | m -> Some (m, m)) x
        |> List.sum

    let modules = File.ReadAllLines ("input.txt") |> Array.map int
    
    Array.sumBy fuelFuel modules



