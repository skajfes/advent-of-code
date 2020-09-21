open System
open System.Text.RegularExpressions

type Reindeer = {
    Name: string
    Speed: int
    FlyTime: int
    RestTime: int
}

let makeReindeer (name, speed, flyTime, restTime) =
    { Name = name; Speed = speed; FlyTime = flyTime; RestTime = restTime }

let parse line =
    // Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
    let m = Regex.Match(line, "^(\w+) .* (\d+) .* (\d+) .* (\d+)")
    let g = m.Groups
    makeReindeer (g.[1].Value, g.[2].Value |> int, g.[3].Value |> int, g.[4].Value |> int)
    
let parseInput input =
    Seq.map parse input
    
let distance deer =
    seq {
        for period in 0..Int32.MaxValue do
            for t in 1..deer.FlyTime -> deer.Speed * (t + period * deer.FlyTime)
            for _ in 1..deer.RestTime -> deer.Speed * ((period + 1) * deer.FlyTime)
    }
    
let maxDistance time reindeer =
    reindeer
    |> Seq.map (fun deer -> (deer.Name, distance deer |> Seq.skip (time - 1) |> Seq.head))
    |> Seq.maxBy snd

let maxPoints time reindeer =
    [1..time]
    |> Seq.map (fun t -> maxDistance t reindeer)
    |> Seq.groupBy fst
    |> Seq.map (fun (d, distances) -> (d, Seq.length distances))
    |> Seq.maxBy snd
    

[<EntryPoint>]
let main argv =
    let reindeer = IO.File.ReadAllLines("input.txt") |> parseInput
    
    maxDistance 2503 reindeer |> printfn "part1: %A"
    maxPoints 2503 reindeer |> printfn "part2: %A"
    
    0
