
open System

type Moon = {
    Position: int*int*int
    Speed: int*int*int
}

let createMoon str =
    // <x=-1, y=0, z=2>
    let r = Text.RegularExpressions.Regex(@"x=(?<x>-?\d+), y=(?<y>-?\d+), z=(?<z>-?\d+)")
    let m = r.Match(str)
    let x = m.Groups.["x"].Value |> int
    let y = m.Groups.["y"].Value |> int
    let z = m.Groups.["z"].Value |> int
    { Position = (x, y, z); Speed = (0, 0, 0)}

let createMoons input =
    Seq.map createMoon input
    |> Seq.toList

let printMoons moons =
    List.iter (fun m -> printfn "Position = %15s; Speed = %15s" (sprintf "%A" m.Position) (sprintf "%A" m.Speed)) moons
    moons

let positionDiff (x1, y1, z1) (x2, y2, z2) = 
    let diff a b =
        if a = b then 0
        else if a < b then 1
        else -1
    (diff x1 x2, diff y1 y2, diff z1 z2)

let addSpeed (x1, y1, z1) (x2, y2, z2) =
    (x1 + x2, y1 + y2, z1 + z2)

let rec simulate count moons =
    if count = 0 then moons else

    let applyGravity moon =
        let speed = 
            moons
            |> List.map (fun m -> m.Position)
            |> List.fold (fun s -> positionDiff moon.Position >> addSpeed s ) moon.Speed 
        { moon with Speed = speed }

    let applySpeed moon =
        let position = 
            addSpeed moon.Position moon.Speed
        { moon with Position = position}

    List.map (applyGravity >> applySpeed) moons
    |> simulate (count - 1)

let energy moons =
    let moonEnergy m =
        let sum (x, y, z) = (abs x) + (abs y) + (abs z)
        let pot = sum m.Position
        let kin = sum m.Speed
        pot * kin
    moons
    |> List.sumBy moonEnergy


let fst (a, _, _) = a
let snd (_, b, _) = b
let trd (_, _, c) = c

let equals f m1 m2 =
    let f m = List.map (fun m -> (f m.Position, f m.Speed)) m
    f m1 = f m2

let findSame moons =
    let rec doFind m cnt f =
        let m = simulate 1 m
        if equals f m moons
        then (cnt + 1)
        else doFind m (cnt + 1) f
    let x = doFind moons 0 fst
    let y = doFind moons 0 snd
    let z = doFind moons 0 trd
    (x, y, z)

let rec gcd a b =
    match b with
    | b when a = b -> a
    | b when a > b -> gcd (a - b) b
    | b -> gcd a (b - a)

let lcm a b =
    if a = 0L && b = 0L then 0L
    else (abs a / gcd a b) * abs b


[<EntryPoint>]
let main argv =

    let moons =
        IO.File.ReadAllLines("input.txt")
        |> createMoons

    // part 1
    // simulate 1000 moons
    // |> printMoons
    // |> energy

    let c = findSame moons
    
    (fst >> int64) c 
    |> lcm <| (snd >> int64) c
    |> lcm <| (trd >> int64) c
    |> printfn "%d"

    0
    

    