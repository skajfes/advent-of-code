
let sumOfDivisors n filter =
    let max = float n |> sqrt |> int
    [1..max]
    |> List.filter (fun x -> n % x = 0)
    |> List.collect (fun x -> [x; n/x])
    |> List.distinct
    |> List.filter filter
    |> List.sum
    
let presentsForHouse n =
    sumOfDivisors n (fun _ -> true)
    |> (*)10

let presentsForHouse' n =
    sumOfDivisors n (fun x -> x * 50 >= n)
    |> (*)11
    
let findHouseWithPresents target f =
    Seq.unfold (fun n -> Some (n+1, n+1)) 1
    |> Seq.filter (fun x -> f x > target)
    |> Seq.head
    
[<EntryPoint>]
let main argv =
    findHouseWithPresents 29000000 presentsForHouse |> printfn "Part 1: %A"
    findHouseWithPresents 29000000 presentsForHouse' |> printfn "Part 2: %A"
    
    0 // return an integer exit code
