let countTriangles tri =
    tri
    |> Seq.map (fun (r: string) ->
        let [| a; b; c |] =
            r.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Array.sort
        (a, b, c))
    |> Seq.filter (fun (a, b, c) -> a + b > c)
    |> Seq.length
    
let countTrianglesByColumns input =
    input
    |> Seq.map (fun (r: string) ->
        r.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int )
    |> Seq.chunkBySize 3
    |> Seq.map (fun [| a; b; c |] ->
        [for i in 0..2 -> (a.[i], b.[i], c.[i])])
    |> Seq.concat
    |> Seq.filter (fun (a, b, c) -> a + b > c && a + c > b && b + c > a)
    |> Seq.length
   
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt") 
    input |> countTriangles |> printfn "Part 1: %d"
    input |> countTrianglesByColumns |> printfn "Part 2: %d"
    0 // return an integer exit code