open System

let toAsteroids (input: string) = 
    input.Split("\n")
    |> Seq.mapi (fun i row -> Seq.mapi (fun j col -> ((j, i), col)) row)
    |> Seq.concat
    |> Seq.toList
    |> List.filter (snd >> (=)'#')
    |> List.map fst


let inLine (x1, y1) (x2, y2) (x:int, y:int) =
    if x1 = x2 
    then x = x1
    else 
        let a = float (y2 - y1) / float (x2 - x1)
        let b = (float y1) - a * (float x1)
        float y = System.Math.Round(a * (float x) + b, 4)

let sameDirection (x1, y1) (x2, y2) (x, y) =
    let sx =
        if x2 >= x1 
        then x >= x1
        else x < x1
    let sy =
        if y2 >= y1 
        then y >= y1
        else y < y1
    sx && sy

let angle (x1, y1) (x2, y2) =
    match Math.Atan2 (float (x2 - x1), float (y1 - y2)) with
    | a when a < 0.0 -> a + 2.0 * Math.PI
    | a -> a

let rec canSee asteroids a1 =
    match List.filter ((<>)a1) asteroids with
    | [] -> []
    | a2::otherAsteroids -> 
        let isInLine a = inLine a1 a2 a && sameDirection a1 a2 a
        let rest = List.filter (isInLine >> not) otherAsteroids
        let distance a = pown (fst a - fst a1) 2 + pown (snd a - snd a1) 2
        let inLine = 
            List.filter (isInLine) (a2::otherAsteroids)
        let closest = List.minBy distance inLine

        closest :: (canSee rest a1)

let maxVisible asteroids = 
    List.map (fun a -> (a, canSee asteroids a |> List.length)) asteroids
    |> List.maxBy snd

let rec vaporize asteroids station =
    match List.filter ((<>) station) asteroids with
    | [] -> []
    | asteroids ->
    let vaporized = 
        canSee asteroids station
        |> List.sortBy (angle station)
    let rest = List.filter (fun a -> List.contains a vaporized |> not) asteroids
    List.append vaporized (vaporize (rest) station)


[<EntryPoint>]
let main argv =

    let asteroids =
        IO.File.ReadAllText("input.txt") 
        |> toAsteroids 
    
    let station = maxVisible asteroids
    // part 1
    printfn "%d" (snd station)

    // part 2
    vaporize asteroids (fst station)
    |> List.skip 199
    |> List.head
    |> (fun (x,y) -> 100*x+y)


