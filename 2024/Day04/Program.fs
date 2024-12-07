open System.IO
open System.Text.RegularExpressions

let parse (input: string[]) =
    let n = Array.length input
    let coords =
        input |> Array.mapi (fun r row -> row |> Seq.mapi (fun c v -> (r, c), v))
        |> Seq.concat
        |> Map.ofSeq
    n, coords

let find s i =
    Regex.Matches(i, s) |> Seq.map _.Groups[0].Index |> Seq.map ((+)1)

let rows n =
    [for r in 0..(n-1) do [for c in 0..n-1 do r, c]]
let cols n =
    [for c in 0..(n-1) do [for r in 0..n-1 do r, c]]
let diags n =
    [for p in 0..(2*n-1) do [for q in (max 0 (p-n+1))..(min p (n-1)) do (q, p-q)]]
let diags2 n =
    [for p in 0..(2*n-1) do [for q in (max 0 (p-n+1))..(min p (n-1)) do (n-1-q, p-q)]]

let findMasDiag search searchRev diags coords =
    diags
    |> List.map (fun d ->
                        let cs = d
                                |> List.map (fun x -> x, Map.find x coords)
                        let is = cs
                                |> List.map (fun (x, e) -> e)
                                |> List.toArray
                                |> System.String
                                |> fun x -> Seq.append (find search x) (find searchRev x) |> Seq.toList
                        is |> List.map (fun i -> cs.Item i |> fst)
                        )
    |> List.concat

let part1 (n, cs) =
    let d1 = cs |> findMasDiag "XMAS" "SAMX" (diags n) |> Seq.length
    let d2 = cs |> findMasDiag "XMAS" "SAMX" (diags2 n) |> Seq.length
    let d3 = cs |> findMasDiag "XMAS" "SAMX" (rows n) |> Seq.length
    let d4 = cs |> findMasDiag "XMAS" "SAMX" (cols n) |> Seq.length

    d1+d2+d3+d4

let part2 (n, cs) =
    let d1 = cs |> findMasDiag "MAS" "SAM" (diags n) |> Set.ofSeq
    let d2 = cs |> findMasDiag "MAS" "SAM" (diags2 n) |> Set.ofSeq

    Set.intersect d1 d2 |> Set.count

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
