// Learn more about F# at http://fsharp.org

open System
open System.Text.Json

let rec sum notRed (el: JsonElement) =
    match el.ValueKind with
    | JsonValueKind.Number -> el.GetInt32()
    | JsonValueKind.Array -> el.EnumerateArray() |> Seq.sumBy (sum notRed)
    | JsonValueKind.Object ->
        let isRed =
            el.EnumerateObject()
            |> Seq.filter (fun prop -> prop.Value.ValueKind = JsonValueKind.String)
            |> Seq.exists (fun prop -> prop.Value.GetString() = "red")
        if notRed && isRed then 0 else
        el.EnumerateObject() |> Seq.sumBy (fun prop -> sum notRed prop.Value)
    | _ -> 0
    
    
[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllText("input.txt")
    let json = JsonDocument.Parse(input)
    
    sum false json.RootElement |> printfn "part1: %A"
    sum true json.RootElement |> printfn "part2: %A"
    
    0 // return an integer exit code
