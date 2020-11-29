open System.Collections.Generic
open FSharpx.Collections

type Player = {
    Name: string
    Damage: int
    Armor: int
    HitPoints: int
    Mana: int
}

let newPlayer name = {
    Name = name
    Damage = 0
    Armor = 0
    HitPoints = 100
    Mana = 0
}

type Spell =
    | MagicMissle
    | Drain
    | Shield
    | Poison
    | Recharge

type Effect = Spell * int

type RoundResult =
    | Alive of Player * Player * Effect list
    | PlayerWins
    | BossWins of string
    
let spells = [
    (MagicMissle, 53)
    (Drain, 73)
    (Shield, 113)
    (Poison, 173)
    (Recharge, 229)
]

let cost spell =
    List.where (fst >> (=)spell) spells |> List.head |> snd

let (>>=) input switchFunction =
    match input with
    | Alive (p1, p2, es) -> switchFunction p1 p2 es
    | _ -> input
    
let checkState state =
    match state with
    | (p, _, _) when p.Mana < 0 -> BossWins "Player out of mana"
    | (_, b, _) when b.HitPoints <= 0 -> PlayerWins
    | (p, _, _) when p.HitPoints <= 0 -> BossWins "Player out of HP"
    | (p, b, es) -> Alive (p, b, es)
    
let debug format =
//    printfn format
    sprintf format

let runEffects p b effects =
    let runEffect (p:Player, b:Player, es:Effect list) (spell, timer) =
        // reduce effect timer
        let e = (spell, timer - 1)
        let es = if snd e > 0 then e::es else es
        
        match spell with
        | Shield ->
            debug "Shield's timer is now %d" (snd e) |> ignore
            let p = { p with Armor = if snd e > 0 then 7 else 0 }
            (p, b, es)
        | Poison ->
            debug "Poison deals 3 damage; its timer is now %d" (snd e) |> ignore
            let b = { b with HitPoints = b.HitPoints - 3 }
            (p, b, es)
        | Recharge ->
            debug "Recharge provides 101 mana; its timer is now %d" (snd e) |> ignore
            let p = { p with Mana = p.Mana + 101 }
            (p, b, es)
        | _ -> failwith "unknown effect"
                                
    let state = List.fold runEffect (p, b, []) effects
    
    checkState state
    
let runSpell spell (p:Player) (b: Player) (effects: Effect list) =
    debug "Player casts %A" spell |> ignore
    let p = { p with Mana = p.Mana - (cost spell) }
    
    let p, b, e = 
        match spell with
        | MagicMissle ->
            let b = { b with HitPoints = b.HitPoints - 4 }
            (p, b, None)
        | Drain ->
            let p = { p with HitPoints = p.HitPoints + 2 }
            let b = { b with HitPoints = b.HitPoints - 2 }
            (p, b, None)
        | Shield ->
            let p = { p with Armor = 7 }
            (p, b, Some (Shield, 6))
        | Poison ->
            (p, b, Some (Poison, 6))
        | Recharge ->
            (p, b, Some (Recharge, 5))
        
    match e with
    | Some (e, _) when List.exists (fun e' -> e = fst e') effects ->
        BossWins "Player applied spell with existing effect"
    | Some e -> checkState (p, b, e::effects)
    | None -> checkState (p, b, effects)
        
let damageDealt (attacker: Player) (defender: Player) =
    let score = attacker.Damage - defender.Armor
    // damage at least 1
    System.Math.Max(1, score)
    
let attack p1 p2 effects =
    let d = damageDealt p2 p1
    let p1 = { p1 with HitPoints = p1.HitPoints - d }
    debug "Boss attacks for %d damage" d |> ignore
    checkState (p1, p2, effects)
    
let oneDamage p1 p2 effects =
    let p1 = { p1 with HitPoints = p1.HitPoints - 1 }
    debug "Player loses 1 HP" |> ignore
    checkState (p1, p2, effects)
        
let noop p1 p2 effects =
    Alive (p1, p2, effects)
    
let show s p1 p2 effects =
    debug "\n-- %s turn --" s |> ignore
    debug "- Player has %d hit points, %d armor, %d mana" p1.HitPoints p1.Armor p1.Mana |> ignore
    debug "- Boss has %d hit points" p2.HitPoints |> ignore
    Alive (p1, p2, effects)
    
let round roundDamage spell p b effects =
    Alive (p, b, effects)
    >>= show "Player"
    >>= roundDamage 
    >>= runEffects
    >>= runSpell spell
    >>= show "Boss"
    >>= runEffects
    >>= attack

let findMinMana round p b =
    
    let rec find (visited: HashSet<int * RoundResult>) queue =
        if PriorityQueue.isEmpty queue then 0 else
        
        let (mana, state), queue = PriorityQueue.pop queue
        
        if visited.Contains(mana,state) then find visited queue else
            
        match state with
        | PlayerWins -> mana
        | BossWins _ -> find visited queue
        | Alive _ -> 
            visited.Add(mana, state) |> ignore
            
            let queue = 
                spells
                |> List.map (fun (spell, cost) -> mana + cost, state >>= round spell)
                |> List.fold (fun q s -> PriorityQueue.insert s q) queue
            
            find visited queue
        
    PriorityQueue.empty false
    |> PriorityQueue.insert (0, Alive (p, b, []))
    |> find (HashSet<int * RoundResult>())
    
    
[<EntryPoint>]
let main argv =
    let p1 = { newPlayer "Player" with HitPoints = 50; Mana = 500 }
    let boss = { newPlayer "Boss" with HitPoints = 71; Damage = 10 }
    
    findMinMana (round noop) p1 boss |> printfn "Part1: %A"
    findMinMana (round oneDamage) p1 boss |> printfn "Part2: %A"
    
//    [Recharge; Recharge; Poison; Shield; Poison; Poison; Shield; Poison; MagicMissle; MagicMissle]
//    |> List.fold (fun r s -> r >>= round s) (Running (p1, boss, []))
//    |> printfn "%A"
    
    0 