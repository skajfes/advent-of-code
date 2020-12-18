
type Ops =
    | Op of char
    | Digit of int64
    | Open
    | Close
    
type Stack =
    | D of int64
    | Add of int64
    | Mul of int64
    | Scope
    
    
let parse (input: string) =
        
    input
    |> Seq.fold (fun (buf, ops) ->
         function
         | ' ' -> "", if buf.Length > 0 then ops @ [Digit (int64 buf)] else ops
         | op when "+*".Contains(op) -> buf, ops @ [Op op]
         | '(' -> buf, ops @ [Open]
         | ')' -> "", if buf.Length > 0 then ops @ [Digit (int64 buf); Close] else ops @ [Close]
         | d when System.Char.IsDigit(d) -> buf + string d, ops
         | op -> failwithf "unknown op %A" op) ("", [])
    |> fun (buf, ops) -> if buf.Length > 0 then ops @ [Digit (int64 buf)] else ops

    
let calc precenence ops =
    
    let rec doCalc (stack: Stack list) (ops:Ops list) =
        // printfn "%A %A" ops res
        match ops, stack with
        | [], [D d] -> d
        | _ ->
            match ops, stack with
            | [], D d::Scope::res -> res, [Digit d]
            | Digit d::ops, [] -> [D d], ops
            | Digit d::ops, Add a::res -> res, Digit (a + d)::ops
            | Digit d::Op '+'::ops, Mul a::res when precenence -> D d::Mul a:: res, Op '+'::ops
            | Digit d::ops, Mul a::res -> res, Digit (a * d)::ops
            | Op '+'::ops, D d::res -> Add d::res, ops
            | Op '*'::ops, (D d)::res -> Mul d::res, ops
            | Open::ops, res -> Scope::res, ops
            | Close::ops, D d::Scope::res -> res, Digit d::ops
            | Digit d::ops, Scope::res -> D d::Scope::res, ops
            | _ -> failwith "error"
            ||> doCalc

    doCalc [] ops
    
let sum precedence =
    Array.map (parse)
    >> Array.map (calc precedence)
    >> Array.sum
    
[<EntryPoint>]
let main argv =
    
    let input = System.IO.File.ReadAllLines("input.txt")
    
    input |> sum false |> printfn "Part 1: %d"
    input |> sum true |> printfn "Part 2: %d"
    
    0 // return an integer exit code