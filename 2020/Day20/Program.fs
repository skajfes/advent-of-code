open System.Diagnostics
open System.IO

let testInput = File.ReadAllText("sample.txt")

type Matrix<'a> = 'a [] []
type Tile = int * Matrix<char>

let parse (input: string) =
    input.Split("\r\n\r\n")
    |> Seq.filter ((<>)"")
    |> Seq.map (fun input ->
        let id :: tiles = input.Split("\r\n") |> Array.toList
        (id.Replace("Tile ", "").Replace(":", "") |> int,
         (Seq.map (Seq.toArray) >> Seq.toArray) tiles) )
    |> Seq.toList


let rotate (data:char [][]) =
    let d = Array.length data
    [| for i in 0 .. d - 1 -> [| for j in 0 .. d - 1 -> data.[d - 1 - j].[i] |] |]
let flip (data: char [][]) =
    let d = Array.length data
    [| for i in 0 .. d - 1 -> [| for j in 0 .. d - 1 -> data.[i].[d - 1 - j] |] |]

let allImagePositions image =
    [id; rotate; rotate; rotate; flip ; rotate; rotate; rotate]
    |> Seq.mapFold (fun image f ->
        let i = f image
        i, i) image
    |> fst

let allPositions tile =
    let id, data = tile
    allImagePositions data
    |> Seq.map (fun d -> id, d)
    |> Seq.toList

let rightEdge ((_, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[i].[d] |]

let leftEdge ((_, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[i].[0] |]

let topEdge ((_, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[0].[i] |]

let bottomEdge ((_, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[d].[i] |]

let equals ((a, _):Tile) ((b, _):Tile) = a = b

let assembleImage tiles =

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
        | Some t -> (dir, t) :: image, rest
        | None -> image, rest
    
    let addRight (x, y) = (x, y + 1)
    let addLeft (x, y) = (x, y - 1)
    let addTop (x, y) = (x - 1, y)
    let addBottom (x, y) = (x + 1, y)
    
    let rec findEdges image tiles =
        let findAndAdd filter dir image tiles =
            let (_, mTile, tiles) = findNextTile filter image tiles
            let (image, tiles) = addToImage (dir) image mTile tiles
            
            match mTile with
            | Some t -> findEdges image tiles
            | None -> (image, tiles)
            
        let (coord, tile) = List.head image
        // printfn "findEdges %A %A" coord (List.length tiles)
            
        (image, tiles)
        ||> findAndAdd (leftEdge >> (=) (rightEdge tile)) (addRight coord)
        ||> findAndAdd (rightEdge >> (=) (leftEdge tile)) (addLeft coord)
        ||> findAndAdd (topEdge >> (=) (bottomEdge tile)) (addBottom coord)
        ||> findAndAdd (bottomEdge >> (=) (topEdge tile)) (addTop coord)
        
    let tile::tiles = tiles
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
    
    [(minx, miny);(minx, maxy); (maxx, miny); (maxx, maxy)]
    |> List.map (fun x -> image |> (List.find (fst >> (=) x) >> snd >> fst >> int64))
    |> List.fold (*) 1L

let removeBorder tile =
    let (coord, (id, data)) = tile
    
    let d = Array.length data
    let data = 
        data
        |> Array.skip 1
        |> Array.take (d-2)
        |> Array.map (
            Array.skip 1 >> Array.take (d-2))
    
    (coord, (id, data))
    
let join (image: ((int*int)*Tile) list) =
    let xs = image |> List.map (fst >> fst)
    let ys = image |> List.map (fst >> snd)
    let minx = List.min xs
    let maxx = List.max xs
    let miny = List.min ys
    let maxy = List.max ys
    
    let image = image |> List.map (removeBorder)

    let d = image |> List.head |> snd |> snd |> Array.length
    [| for x in 0..(maxx-minx+1)*d - 1 ->
       [| for y in 0..(maxy-miny+1)*d - 1 ->
             let tilex = (x/d + minx, y/d + miny)
             let tile = image |> List.find (fst >> (=)tilex) |> snd
             (snd tile).[x%d].[y%d] |] |]
             
let printImage image =
    image
    |> Seq.fold (fun out row ->
        Seq.map (string) row
        |> System.String.Concat
        |> fun s -> out + s + "\n") "" 
    |> printfn "%s"
             
let flatten image =
    image
    |> Seq.mapi (fun i -> Seq.mapi (fun j c -> (i, j), c))
    |> Seq.concat
    |> Seq.filter (snd >> (=)'#')
    |> Seq.map fst
    |> Seq.toList
    
let monster =
    File.ReadAllLines("monster.txt")
    |> flatten
    |> List.map (fun (i,j) -> i-1,j) // normalize so monster as a field on (0,0)
         
let findMonster (image: char[][])=
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
    |> Array.concat
    |> Array.map (function
        |'#' -> 1
        | _ -> 0)
    |> Array.sum
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

    printfn "Elapsed parse: %d\tpart1: %d\tpart2: %d %d" t_parse t_part1 t_part2_1 t_part2

    0 // return an integer exit code
