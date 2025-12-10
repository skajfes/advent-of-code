open System.IO

let parse (input: string[]) = 
    input
    |> Array.map (fun s -> s.Split(","))
    |> Array.map (fun [| x; y |] -> int64 x, int64 y)

let area (xa, ya) (xb, yb) =
    (abs (xb-xa) + 1L) * (abs (yb - ya) + 1L)


let intersects ((ax, ay), (bx, by)) ((lax, lay), (lbx, lby)) =

    // normalize points of square - a always top-left, b always bottom-right
    let ax, bx  = if ax <= bx then ax, bx else bx, ax
    let ay, by = if ay <= by then ay, by else by, ay

    // normalize points of line - lax always left, lbx always right, or up/down if vertical
    let lax, lbx = if lax <= lbx then lax, lbx else lbx, lax
    let lay, lby = if lay <= lby then lay, lby else lby, lay

    // line outside of square
    let touching = 
        if lax = lbx then
            if lax < ax || lax > bx then false else
            if lay < ay && lby < ay then false else
            if lay > by && lby > by then false else
            true
        else 
            if lay < ay || lay > by then false else
            if lax < ax && lbx < ax then false else
            if lax > bx && lbx > bx then false else
            true

    if not touching then false else

    let res =

        // has points within the square - true
        if lax > ax && lax < bx && lay > ay && lby < by then true else
        if lbx > ax && lbx < bx && lay > ay && lby < by then true else
        // has both points on the edges of square
        if lay = lby && lax = ax && lbx = bx && lay > ay && lay < by then true else
        if lax = lbx && lay = ay && lby = by && lax > ax && lax < bx then true else

        let testLine (ax, ay) (bx, by) =
            // - if square line is vertical && line is vertical -> false
            // - if squere line is horizontal && line is horizontal -> false
            if ax = bx && lax = lbx then false else // both lines vertical
            if ay = by && lay = lby then false else // both lines horizontal
            if ax = bx then
                // vertical line
                //    - if y between a&b && sq x between lax&lbx -> true
                if ay < lay && by > lay && lax < ax && ax < lbx then true else false
            else
                // horizontal line
                //    - if x between a&b && sq y between lay&lby -> true
                if ax < lax && bx > lax && lay < ay && ay < lby then true else false

        // test each line of square
        [
            testLine (ax, ay) (bx, ay)
            testLine (bx, ay) (bx, by)
            testLine (ax, by) (bx, by)
            testLine (ax, ay) (ax, by)
        ]
        |> List.exists ((=) true)

    //printfn "%d %A intersectes %A = %A" (area (ax, ay) (bx, by)) ((ax, ay), (bx, by)) ((lax, lay), (lbx, lby)) res
    res
    

let part1 tiles =
    tiles
    |> Array.mapi (fun i a -> tiles[i+1..] |> Array.map (area a))
    |> Array.concat
    |> Array.max

let part2 tiles =
    let lines =
        Array.append tiles [|tiles[0]|]
        |> Array.windowed 2
        |> Array.map (fun [|a; b|] -> a, b)

    tiles
    |> Array.mapi (fun i a -> tiles[i+1..] |> Array.map (fun b -> (a, b), area a b))
    |> Array.concat
    |> Array.sortByDescending snd
    |> Seq.filter (fun (square, _) -> lines 
                                      |> Seq.forall (fun line -> not (intersects square line)) 
                                      )
    |> Seq.head 
    |> snd

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    let testInput = File.ReadAllLines("sample.txt")
    
    testInput |> parse |> part1 |> printfn "%A"
    testInput |> parse |> part2 |> printfn "%A"

    input |> parse |> part1 |> printfn "Part 1: %d"
    input |> parse |> part2 |> printfn "Part 2: %d"

    0 // return an integer exit code
