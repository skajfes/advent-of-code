open System.Collections.Generic
let testInput = System.IO.File.ReadAllText("sample.txt")

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

let rotateI (data:char [][]) =
    let d = Array.length data
    [| for i in 0 .. d - 1 -> [| for j in 0 .. d - 1 -> data.[d - 1 - j].[i] |] |]
let flipI (data: char [][]) =
    let d = Array.length data
    [| for i in 0 .. d - 1 -> [| for j in 0 .. d - 1 -> data.[i].[d - 1 - j] |] |]

let allImagePositions image =
    [ image
      image |> rotateI
      image |> rotateI |> rotateI
      image |> rotateI |> rotateI |> rotateI
      image |> flipI
      image |> flipI |> rotateI
      image |> flipI |> rotateI |> rotateI
      image |> flipI |> rotateI |> rotateI |> rotateI ]
let allPositions tile =
    let id, data = tile
    allImagePositions data
    |> List.map (fun d -> id, d)

let rightEdge ((id, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[i].[d] |]

let leftEdge ((id, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[i].[0] |]

let topEdge ((id, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[0].[i] |]

let bottomEdge ((id, data): Tile) =
    let d = Array.length data - 1
    [| for i in 0 .. d -> data.[d].[i] |]

let equals ((a, _):Tile) ((b, _):Tile) = a = b

let assembleImage tiles =

    let findNextTile filter image tiles =
        let matchingTile = 
            tiles
            |> List.map (allPositions)
            |> List.map (List.filter filter)
            |> List.tryFind (List.length >> (<) 0)
            |> function
               | Some t -> List.head t |> Some
               | None -> None
            
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
    """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """.Split("\r\n")
 |> flatten
 |> List.map (fun (i,j) -> i-1,j) // normalize so monster as a field on (0,0)
         
let findMonster (image: char[][])=
    allImagePositions image
    |> List.map flatten
    |> List.map (fun image ->
        image
        |> List.map (fun (i, j) ->
              monster
              |> List.map (fun (ii, jj) -> i + ii, j + jj)
              |> List.forall(fun m -> image |> List.exists ((=) m)))
        |> List.filter ((=)true)
        |> List.length)
    |> List.sum

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
    let input = System.IO.File.ReadAllText("input.txt")

    let image = (parse >> assembleImage) input
        
    image |> multiplyCorners |> printfn "Part 1: %d"
    image |> join |> measureWaters |> printfn "Part 2: %d"

    0 // return an integer exit code
