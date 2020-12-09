open System.IO
open System.Text.RegularExpressions

let testInput = [|
    "aaaaa-bbb-z-y-x-123[abxyz]"
    "a-b-c-d-e-f-g-h-987[abcde]"
    "not-a-real-room-404[oarel]"
    "totally-real-room-200[decoy]"
|]

type Entry = {
    Sector: int
    Checksum: char list
    Content: string
}

let parseEntry (e: string) =
    let m = Regex.Match(e, @"^([a-z-]+)-(\d+)\[([a-z]{5})\]$")
    {
        Sector = m.Groups.[2].Value |> int
        Content = m.Groups.[1].Value
        Checksum = m.Groups.[3].Value |> Seq.toList
    }
let sector e = e.Sector

let checksum (s: string) =
    s.Replace("-", "")
    |> Seq.countBy id
    |> Seq.sortByDescending (fun (c, i) -> i, int 'z' - int c)
    |> Seq.take 5
    |> Seq.map fst
    |> Seq.toList
    
let realRoomsSum input =
    input
    |> Seq.map parseEntry
    |> Seq.filter (fun e -> checksum e.Content = e.Checksum)
    |> Seq.map sector
    |> Seq.sum


let decrypt e =
    e.Content
    |> Seq.map (function
        | '-' -> ' '
        | c -> (int c + e.Sector - int 'a') % 26 + int 'a' |> char)
    |> Seq.map string
    |> System.String.Concat, e.Sector
    
let decryptRoomNames input =
    input
    |> Seq.map parseEntry
    |> Seq.filter (fun e -> checksum e.Content = e.Checksum)
    |> Seq.map decrypt
    |> Seq.filter (fst >> (=) "northpole object storage")
    |> Seq.head
    |> snd
    
[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt")
    input |> realRoomsSum |> printfn "Part 1: %A"
    input |> decryptRoomNames |> printfn "Part 2: %A"
    
    0 // return an integer exit code