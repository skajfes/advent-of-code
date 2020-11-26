
type Player = {
    Name: string
    Damage: int
    Armor: int
    HitPoints: int
}

type Item = {
    Name: string
    Cost: int
    Damage: int
    Armor: int
}

type Weapon = Item
type Armor = Item
type Ring = Item

type Inventory =
    Weapon * Armor option * Ring option * Ring option
    
let weapons = [
    { Name = "Dagger"; Cost = 8; Damage = 4; Armor = 0 }
    { Name = "Shortsword"; Cost = 10; Damage = 5; Armor = 0 }
    { Name = "Warhammer"; Cost = 25; Damage = 6; Armor = 0 }
    { Name = "Longsword"; Cost = 40; Damage = 7; Armor = 0 }
    { Name = "Greataxe"; Cost = 74; Damage = 8; Armor = 0 }
]
let armors = [
    { Name = "Leather"; Cost = 13; Damage = 0; Armor = 1 }
    { Name = "Chainmail"; Cost = 31; Damage = 0; Armor = 2 }
    { Name = "Splintmail"; Cost = 53; Damage = 0; Armor = 3 }
    { Name = "Bandedmail"; Cost = 75; Damage = 0; Armor = 4 }
    { Name = "Platemail"; Cost = 102; Damage = 0; Armor = 5 }
]

let rings = [
    { Name = "Damage +1"; Cost = 25; Damage = 1; Armor = 0 }   
    { Name = "Damage +2"; Cost = 50; Damage = 2; Armor = 0 }   
    { Name = "Damage +3"; Cost = 100; Damage = 3; Armor = 0 }   
    { Name = "Defense +1"; Cost = 20; Damage = 0; Armor = 1 }   
    { Name = "Defense +2"; Cost = 40; Damage = 0; Armor = 2 }   
    { Name = "Defense +3"; Cost = 80; Damage = 0; Armor = 3 }   
]
    
let newPlayer name = {
    Name = name
    Damage = 0
    Armor = 0
    HitPoints = 100
}

let player (p:Player) ((w, a, r1, r2):Inventory) =
    let damage =
        function
        | Some i -> i.Damage
        | None -> 0
    
    let armor =
        function
        | Some i -> i.Armor
        | None -> 0
        
    { p with
          Damage = p.Damage + w.Damage + damage a + damage r1 + damage r2
          Armor = p.Armor + w.Armor + armor a + armor r1 + armor r2 }
    
    

let damageDealt (attacker: Player) (defender: Player) =
    let score = attacker.Damage - defender.Armor
    // damage at least 1
    System.Math.Max(1, score)
    
let rec fight (p1: Player) (p2: Player) =
    let d = damageDealt p1 p2
    let p2 = { p2 with HitPoints = p2.HitPoints - d }
//    printfn "%s deals %d damage; %s goes down to %d HP" p1.Name d p2.Name p2.HitPoints
    if p2.HitPoints > 0 then
        fight p2 p1
    else
//        printf "%s wins" p1.Name
        p1.Name = "Player"
    
let allInventories : Inventory seq =
    seq {
       for w in weapons do
           yield (w, None, None, None)
           
           // no armor rings combinations
           for r1 in rings do
               yield (w, None, Some r1, None)
               
               for r2 in List.filter ((<>) r1) rings do
                   yield (w, None, Some r1, Some r2)
                   
           // all armors combinations
           for a in armors do
               yield (w, Some a, None, None)
               
               for r1 in rings do
                   yield (w, Some a, Some r1, None)
                   
                   for r2 in List.filter ((<>) r1) rings do
                       yield (w, Some a, Some r1, Some r2)
           
    }
    
let totalCost ((w, a, r1, r2):Inventory) =
    let cost =
        function
        | Some i -> i.Cost
        | None -> 0
    w.Cost + cost a + cost r1 + cost r2
    
let minForWin p1 boss =
    allInventories
    |> Seq.sortBy totalCost
    |> Seq.where (fun i -> fight (player p1 i) boss)
    |> Seq.head
    |> totalCost
    
let maxForLoss p1 boss =
    allInventories
    |> Seq.sortByDescending totalCost
    |> Seq.filter (fun i -> fight (player p1 i) boss |> not)
    |> Seq.head
    |> totalCost
    
[<EntryPoint>]
let main argv =
    let p1 = newPlayer "Player" 
    let boss = { newPlayer "Boss" with Damage = 8; Armor = 2 }
    
    minForWin p1 boss |> printfn "Part1: min gold for win: %A"
    maxForLoss p1 boss |> printfn "Part2: max gold for loss: %A"
    
    0 