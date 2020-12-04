open System.Text.RegularExpressions
    
type Field =
    | BirthYear | IssueYear | ExpirationYear
    | Height | HairColor | EyeColor
    | PassportId | CountryId 
    
let toField (str:string) =
    match str.Split(':') with
    | [| t; v |] ->
        let code = 
            match t with
            | "byr" -> BirthYear 
            | "iyr" -> IssueYear
            | "eyr" -> ExpirationYear
            | "hgt" -> Height 
            | "hcl" -> HairColor 
            | "ecl" -> EyeColor  
            | "pid" -> PassportId 
            | "cid" -> CountryId
            | _ -> failwith "unknown code"
        (code, v)
    | _ -> failwith "incorrect input"
    
let parse (input: string) =
    let parseField (el:string) =
        el.Split(' ')
        |> Array.map toField
        |> Array.toList
        
    input.Split("\r\n")
    |> Array.fold (fun (res, buf) el ->
        match el with
        | "" -> ((List.concat buf) :: res, []) // empty line - add buffer to result list
        | _ -> (res, parseField el :: buf) // not empty line, add elements to buffer
        ) ([], [])
    |> (fun (res, buf) -> List.concat buf :: res) // append last buffer to result list
    
let requiredFields passport =
    let fields = List.map fst passport
    [ BirthYear; IssueYear; ExpirationYear; Height; HairColor; EyeColor; PassportId ]
    |> List.forall (fun f -> List.contains f fields)

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [for g in m.Groups -> g.Value ]
    else None
    
let between a b x = x >= a && x <= b
let isValid (field, value: string) =
    match field with
    | BirthYear -> value |> int |> between 1920 2002
    | IssueYear -> value |> int |> between 2010 2020
    | ExpirationYear -> value |> int |> between 2020 2030
    | Height ->
        match value with
        | Regex @"^(\d+)(cm|in)$" [ h; v ] -> 
            match v with
            | "in" -> int h |> between 59 76
            | "cm" -> int h |> between 150 193
            | _ -> failwith "invalid measure"
        | _ -> false
    | HairColor -> Regex.IsMatch(value, @"^#[0-9a-f]{6}$")
    | EyeColor -> List.contains value ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    | PassportId -> Regex.IsMatch(value, @"^\d{9}$")
    | CountryId -> true
    
let validFields passport =
    List.forall isValid passport
    
let countValid validator passports =
    passports
    |> List.filter validator
    |> List.length
    
[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllText("input.txt") |> parse
    input |> countValid requiredFields |> printfn "Part 1: %A"
    input |> countValid (fun x -> requiredFields x && validFields x) |> printfn "Part 2: %A"
    
    0 // return an integer exit code