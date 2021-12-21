open System.IO

let parse (input: string[]) = 
    let map = input.[0]
    map, input[2..]
    |> Seq.mapi (fun y -> Seq.mapi (fun x c -> (x, y), c))
    |> Seq.concat
    |> Seq.filter (snd >> (=)'#')
    |> Seq.map fst
    |> Seq.toList

let neighbours (x, y) = [
    (x-1, y-1); (x-1, y); (x-1, y+1)
    (x, y-1); (x, y); (x, y+1)
    (x+1, y-1); (x+1, y); (x+1, y+1)
]

let toDecimal bin =
    (0, bin)
    ||> List.fold (fun dec -> function | 0 -> 2 * dec | 1 -> 2 * dec + 1)

let enhanceMap (alg: string) def (map: string[,]) =
    map
    |> Array2D.mapi (fun x y _ ->
          neighbours (x, y)
          |> List.map (fun (x, y) ->
              if x >= 0 && x < Array2D.length1 map && y >= 0 && y < Array2D.length2 map then
                  if map[x, y] = "#" then 1 else 0
              else def) |> toDecimal |> fun x -> string alg[x])

let printm (map: string[,]) =
    [0..Array2D.length1 map - 1]
    |> List.map (fun x ->
        [0..Array2D.length2 map - 1]
        |> List.map (fun y -> map[x, y])
        |> String.concat "")
    |> String.concat "\n"
    |> printfn "%s\n"
    map


let toMap light =
    let minx = (light |> List.map fst |> List.min) - 5
    let miny = (light |> List.map snd |> List.min) - 5
    let maxx = (light |> List.map fst |> List.max) + 5
    let maxy = (light |> List.map snd |> List.max) + 5

    [minx..maxx]
    |> List.map (fun x ->
         [miny..maxy]
         |> List.map (fun y -> if List.contains (y, x) light then "#" else ".")
    )
    |> array2D

let expand map =
    let x = Array2D.length1 map
    let y = Array2D.length2 map
    let m2 = Array2D.create (x+4) (y+4) "."

    map
    |> Array2D.iteri (fun x y v -> m2[x+2, y+2] <- v)

    m2

let count map =
    [for x in [0..Array2D.length1 map - 1] do for y in [0..Array2D.length2 map - 1] -> x, y]
    |> List.map (fun (x, y) -> if map[x, y] = "#" then 1 else 0)
    |> List.sum

let part1 (map, light) =
    light
    |> toMap
    // |> printm
    |> enhanceMap map 0
    // |> printm
    |> enhanceMap map 1
    // |> printm
    |> count

let part2 (alg, light) =

    let map = toMap light
    (map, [1..25])
    ||> List.fold (fun map c ->
            map
            |> expand
            |> enhanceMap alg 0
            |> enhanceMap alg 1)
    |> count

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    // testInput |> parse |> part2 |> printfn "%A"
    
    input |> parse |> part1 |> printfn "Part 1: %A"
    input |> parse |> part2 |> printfn "Part 2: %A"

    0 // return an integer exit code
