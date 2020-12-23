open System
open System.Diagnostics
open System.IO

let testInput = File.ReadAllText("sample.txt")

type Tile = int * char[,]

let parse (input: string) =
    input.Split("\r\n\r\n")
    |> Seq.filter ((<>)"")
    |> Seq.map (fun input ->
        let data = input.Split("\r\n")
        (data.[0].Replace("Tile ", "").Replace(":", "") |> int,
          array2D data.[1..]) )
    |> Seq.toList


let rotate (data: 'a [,]) =
    Array2D.mapi (fun i j _ -> data.[^j, i]) data

let flip (data: 'a [,]) =
    Array2D.mapi (fun i j _ -> data.[i, ^j]) data

let allImagePositions image =
    [id; rotate; rotate; rotate; flip; rotate; rotate; rotate]
    |> Seq.mapFold (fun image f ->
        let i = f image
        i, i) image
    |> fst

let allPositions (tile: Tile): Tile list =
    let id, data = tile
    allImagePositions data
    |> Seq.map (fun d -> id, d)
    |> Seq.toList

let rightEdge ((_, data): Tile) = data.[*, ^0]
let leftEdge ((_, data): Tile) = data.[*, 0]
let topEdge ((_, data): Tile) = data.[0, *]
let bottomEdge ((_, data): Tile) = data.[^0, *]

let equals ((a, _):Tile) ((b, _):Tile) = a = b
let addRight (x, y) = (x, y + 1)
let addLeft (x, y) = (x, y - 1)
let addTop (x, y) = (x - 1, y)
let addBottom (x, y) = (x + 1, y)

let show (image: ((int*int)*Tile) list) =
    let xs = image |> List.map (fst >> fst)
    let ys = image |> List.map (fst >> snd)
    let minx = List.min xs
    let maxx = List.max xs
    let miny = List.min ys
    let maxy = List.max ys

    Console.SetCursorPosition(0, 0)
    // printfn "\n\n"

    let d = image |> List.head |> snd |> snd |> Array2D.length1

    [| for x in 0 .. (maxx - minx + 1) * d - 1 ->
        [| for y in 0 .. (maxy - miny + 1) * d - 1 ->
            match image |> List.tryFind (fst >> (=) (x / d + minx, y / d + miny)) with
            | Some (_, (_, tile)) -> tile.[x % d, y % d] |> string
            | None -> " " |]
        |> String.concat "" |]
    |> String.concat "\n"
    |> printfn "%s"

let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder x y state (array.[x, y])
    state

let show' (image: char[,]) =
    [|0..image.GetLength(0)-1|]
    |> Array.map (fun i -> image.[i, *] |> Array.map string |> String.concat "" )
    |> String.concat "\n"
    |> printfn "%s\n"

let assembleImage (tiles: Tile list) =

    let findNextTile filter image tiles =
        let matchingTile = 
            tiles
            |> List.filter filter
            |> List.tryHead

        match matchingTile with
        | Some t -> image, Some t, tiles |> List.filter (equals t >> not)
        | None -> image, None, tiles

    let addToImage dir (image: ((int*int)*Tile) list) (tile: Tile option) (rest: Tile list) =
        match tile with
        | Some t ->
            let image = (dir, t) :: image
            // show image
            image, rest
        | None -> image, rest

    let rec findEdges image tiles =
        let findAndAdd filter dir image tiles =
            let (_, mTile, tiles) = findNextTile filter image tiles
            let (image, tiles) = addToImage (dir) image mTile tiles
            
            match mTile with
            | Some t -> findEdges image tiles
            | None -> (image, tiles)
            
        let (coord, tile) = List.head image

        (image, tiles)
        ||> findAndAdd (leftEdge >> (=) (rightEdge tile)) (addRight coord)
        ||> findAndAdd (rightEdge >> (=) (leftEdge tile)) (addLeft coord)
        ||> findAndAdd (topEdge >> (=) (bottomEdge tile)) (addBottom coord)
        ||> findAndAdd (bottomEdge >> (=) (topEdge tile)) (addTop coord)

    let rndIdx = Random().Next(List.length tiles)
    let tile = List.item rndIdx tiles
    let tiles = List.filter (equals tile >> not) tiles
    // let tile::tiles = tiles

    let image = [ ((0, 0), tile) ]
    let tiles =
        tiles
        |> List.map allPositions
        |> List.concat
    
    findEdges image tiles |> fst
   
let multiplyCorners image =

    let xs = image |> List.map (fst >> fst)
    let ys = image |> List.map (fst >> snd)
    let minx = List.min xs
    let maxx = List.max xs
    let miny = List.min ys
    let maxy = List.max ys
    
    [(minx, miny); (minx, maxy); (maxx, miny); (maxx, maxy)]
    |> List.map (fun x -> image |> (List.find (fst >> (=) x) >> snd >> fst >> int64))
    |> List.fold (*) 1L

let removeBorder (tile: (int * int) *Tile) =
    let (coord, (id, data)) = tile
    let data = data.[1..^1, 1..^1]
    (coord, (id, data))

let join (image: ((int*int)*Tile) list) =
    let xs = image |> List.map (fst >> fst)
    let ys = image |> List.map (fst >> snd)
    let minx = List.min xs
    let maxx = List.max xs
    let miny = List.min ys
    let maxy = List.max ys

    let image = image |> List.map (removeBorder)

    let d = image |> List.head |> snd |> snd |> Array2D.length1
    let dim = (maxx - minx) + 1

    let img = Array2D.create ((dim) * d) ((dim) * d) '.'

    image
    |> List.iter (fun ((x, y), tile) ->
        let x = x - minx
        let y = y - miny
        img.[x * d..x * d + d - 1, y * d..y * d + d - 1] <- snd tile)
    img

let printImage image =
    image
    |> Seq.fold (fun out row ->
        Seq.map (string) row
        |> String.Concat
        |> fun s -> out + s + "\n") "" 
    |> printfn "%s"


let flatten (image: char[,])=
    image
    |> foldi (fun i j list c -> if c = '#' then (i, j)::list else list) []

let monster =
    File.ReadAllLines("monster.txt")
    |> array2D
    |> flatten
    |> List.map (fun (i,j) -> i - 1, j) // normalize so monster as a field on (0,0)
         
let findMonster (image: char[,])=
    allImagePositions image
    |> Seq.map flatten
    |> Seq.map (fun image ->
        image
        |> List.map (fun (i, j) ->
              monster
              |> List.map (fun (ii, jj) -> i + ii, j + jj)
              |> List.forall(fun m -> image |> List.exists ((=) m)))
        |> List.filter ((=)true)
        |> List.length)
    |> Seq.filter ((<)0)
    |> Seq.head

let measureWaters image =
    let m = findMonster image
    image
    |> flatten
    |> List.length
    |> fun x -> x - m * (List.length monster)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let sw = Stopwatch.StartNew()

    let image = (parse >> assembleImage) input
    let t_parse = sw.ElapsedMilliseconds
    sw.Restart()

    image |> multiplyCorners |> printfn "Part 1: %d"
    let t_part1 = sw.ElapsedMilliseconds
    sw.Restart()

    let joined = join image
    let t_part2_1 = sw.ElapsedMilliseconds
    sw.Restart()

    joined |> measureWaters |> printfn "Part 2: %A"
    let t_part2 = sw.ElapsedMilliseconds
    sw.Stop()

    printfn "Elapsed assemble: %d\tpart1: %d\tpart2: %d %d" t_parse t_part1 t_part2_1 t_part2

    0 // return an integer exit code
