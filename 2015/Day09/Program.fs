// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharpx.Collections


let parse input =
    let m = Regex.Match(input, @"(\w+) to (\w+) = (\d+)")
    if not m.Success then
        failwith "unable to parse"
    else
        let g = m.Groups
        (g.[1].Value, g.[2].Value, g.[3].Value |> int)


let rec visitNode toVisit distances (visited: HashSet<string list>) shortest =

    let (totalDistance, cities), nodes = PriorityQueue.pop toVisit

    if visited.Contains(cities) then
        visitNode nodes distances visited shortest
    else
        // find all neighbours
        let currentCity::rest = cities
        let neighbours =
            distances
            |> List.filter (fun (a, b, _) -> a = currentCity || b = currentCity)
            |> List.map (fun (a, b, distance) -> if a = currentCity then (b, distance) else (a, distance))
            |> List.filter (fun (city, _) -> List.contains city rest |> not)
            |> List.filter (fun (city, _) -> visited.Contains(city::currentCity::rest) |> not)
            
        if shortest && List.isEmpty neighbours then totalDistance else

        // add to nodes
        let nodes =
            neighbours
            |> List.fold (fun nodes (city, distance) -> PriorityQueue.insert (distance + totalDistance, city::cities) nodes) nodes

        if not shortest && nodes.IsEmpty then totalDistance else
            
        // printfn "current: %A" (currentCity::rest, totalDistance)
        // printfn "  neighbours: %A" (neighbours)
        // printfn "  nodes: %A" (nodes) // |> Seq.filter (fun (cities, _) -> Seq.contains cities visited))
        // Console.ReadKey() |> ignore

        visited.Add(cities) |> ignore
        visitNode nodes distances visited shortest


let initializeQueue longest distances =
    let allCities =
        List.fold (fun all (a, b, _) ->
            all
            |> Set.add a
            |> Set.add b) Set.empty distances
        
    Set.fold (fun nodes city ->
        PriorityQueue.insert (0, [city])  nodes) (PriorityQueue.empty longest) allCities
    
let shortestDistance distances =
    let nodes = initializeQueue false distances
    let visited = HashSet<string list>()
    visitNode nodes distances visited true

let longestDistance distances =
    let nodes = initializeQueue false distances
    let visited = HashSet<string list>()
    visitNode nodes distances visited false
    


[<EntryPoint>]
let main argv =
//    let distances = 
//        """London to Dublin = 464
//            London to Belfast = 518
//            Dublin to Belfast = 141"""
//        |> (fun c -> c.Split('\n'))
//        |> Seq.map parse
//        |> Seq.toList
    
    let distances =
        IO.File.ReadAllLines("input.txt")
        |> Seq.map parse
        |> Seq.toList
    
    shortestDistance distances |> printfn "part1 - shortest distance: %d"
    longestDistance distances |> printfn "part2 - longest distance: %d"

    0 // return an integer exit code
