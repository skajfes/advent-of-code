open System

let cmdRev n = (bigint -1, n - bigint 1)
let cmdCut n c = (bigint 1, n - c) 
let cmdInc (c:bigint) = (c, bigint 0)

let toCommand n (input: string) = 
    match input with
    | _ when input = "deal into new stack" -> cmdRev n
    | _ when input.StartsWith("deal with increment ") -> 
        let c = input.Replace("deal with increment ", "") |> int64 
        cmdInc (bigint c)
    | _ when input.StartsWith("cut ") -> 
        let c = input.Replace("cut ", "") |> int64
        cmdCut n (bigint c)
    | cmd -> sprintf "unknown command: %s" cmd |> failwith


let calc n x (a:bigint, b:bigint) = (((a * x + b) % n) + n) % n
let combine n (a:bigint, b:bigint) (c, d) = (a * c % n, (b * c + d) % n)

let createShuffle (input:string []) n =
    Array.toList input
    |> List.map (toCommand n)
    |> List.fold (combine n) (bigint 1L, bigint 0L)

let doShuffle (input:string[]) n x =
    createShuffle input n |> calc n x

let rec multiplyShuffle res shuffle n t =
    if t = 0L then res else

    let res = 
        if t % 2L = 1L 
        then combine n res shuffle
        else res
    
    let shuffle = combine n shuffle shuffle
    multiplyShuffle res shuffle n (t / 2L)


[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllLines("input.txt")
    
    // part 1

    doShuffle input (bigint 10007L) (bigint 2019L)

    // part 2
    let deckSize = bigint 119315717514047L
    let sh = createShuffle input deckSize
    let fsh = multiplyShuffle (bigint 1L, bigint 0L) (sh) deckSize 101741582076661L 

    
    // check
    calc deckSize (bigint 32376123569821L) fsh

    0 // return an integer exit code

