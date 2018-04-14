open System
open System.Collections.Generic
type Stats = Map<string, obj>
type Name = string
#r "Newtonsoft.Json.dll"
// Lens code originally based on http://www.fssnip.net/7Pk/title/Polymorphic-lenses by Vesa Karvonen

type Lens<'s,'t,'a,'b> = ('a -> Option<'b>) -> 's -> Option<'t>
module Lens =
  let inline view l s =
    let r = ref Unchecked.defaultof<_>
    s |> l (fun a -> r := a; None) |> ignore
    !r

  let over l f =
    l (f >> Some) >> function Some t -> t | _ -> failwith "Impossible"
  let inline set l b = over l <| fun _ -> b
  let lens get set = fun f s ->
    (get s |> f : Option<_>) |> Option.map (fun f -> set f s)
  let inline constant c = 
    lens (fun _ -> c) (fun _ v -> v)

module Props =
  let propBase<'t> (propName: string) fallback f =
    Lens.lens
      (fun x -> match Map.tryFind propName x with | Some(:? 't as v:obj) -> v | _ -> fallback x)
      (fun (v:'t) x -> Map.add propName (box v) x)
      f
  let prop (propName: string) f =
    Lens.lens
      (fun x -> match Map.tryFind propName x with | Some(:? 't as v:obj) -> (v |> unbox<'t>) | Some v -> failwithf "Could not convert %A to %s" v (typeof<'t>.Name) | None -> Unchecked.defaultof<'t>)
      (fun (v:'t) x -> Map.add propName (box v) x)
      f
  let singleList f =
    Lens.lens
      (fun v -> 
        match v with
        | head::_ -> 
          head 
        | _ -> 
          Unchecked.defaultof<_>)
      (fun v _ -> [v])
      f
  let inline set prop v stats = stats |> Lens.set prop v
  let inline get prop stats = stats |> Lens.view prop
  let inline propWithFallback (propName: string) fallbackLens f =
    propBase propName (get fallbackLens) f

module DataTypes =
  type DieRoll = DieRoll of number: int * dieSize: int | StaticModifier of int
  type DamageType = Weapon of magical: bool | Fire | Poison
  type DamageDefinition = { damageType: DamageType; amount: DieRoll list }
  type AttackDefinition = { name: string; toHit: int; damages: DamageDefinition list }
    with
    static member Simple name toHit n d plus =
      { name = name; toHit = toHit; damages = [{ damageType = Weapon(false); amount = if plus <> 0 then [DieRoll(n, d); StaticModifier(plus)] else [DieRoll(n,d)] }] }
open DataTypes

module Creature =
  open Props
  open DataTypes
  type Prop<'t> = Lens<Map<string, obj>, Map<string, obj>, 't, 't>
  let name = prop<Name> "Name"
  let cachedQuery propName (converter: string -> 't option) =
    let mutable sessionCache = Map.empty
    let rec getValue name =
      printfn "Enter %s for %s: " propName name
      match (Console.ReadLine().Trim()) |> converter with
      | Some(v) -> v
      | None -> 
        printfn "Sorry, I could not convert that to %s" (typeof<'t>.Name)
        getValue name 
    Lens.lens
      (fun creature ->
        let name = get name creature
        match Map.tryFind (name, propName) sessionCache with
        | Some(v) -> v
        | None ->
          let v = getValue name
          sessionCache <- sessionCache.Add ((name, propName), v)
          v
        )
      (fun (v:'t) x -> Map.add propName (box v) x)
      
  let propWithQueryCache converter propName =
    let readonlyLens = prop propName
    let writeLens = propWithFallback propName (cachedQuery propName converter)
    fun next v ->
      match readonlyLens next v with
      | Some(v) ->
        // it's setting. Don't ask.
        Some v
      | None ->
        // it's getting. Return
        writeLens next v
  let propInt propName =
    propWithQueryCache (fun input -> match System.Int32.TryParse input with | true, v -> Some v | _ -> None) propName
  let propStr propName =
    propWithQueryCache (fun input -> if System.String.IsNullOrWhiteSpace(input) then None else Some input) propName
  let maxHp = propInt "MaxHP"
  let str = propInt "Str"
  let dex = propInt "Dex"
  let con = propInt "Con"
  let int = propInt "Int"
  let wis = propInt "Wis"
  let cha = propInt "Cha"
  let ac = propInt "AC"
  let hp : Prop<int> = propWithFallback "HP" maxHp
  let attacks : Prop<AttackDefinition list> = propWithFallback "Attacks" (Lens.constant [])
  let attack : Prop<AttackDefinition> = singleList >> attacks
  let create name' =
    Map.empty |> set name name'

module Menagerie =
  open System.IO
  open Newtonsoft.Json
  let mutable saveDir = 
    if Directory.Exists(@"C:\one\OneDrive\dnd\chars") then @"C:\one\OneDrive\dnd\chars"
    else """C:\Users\maxw\OneDrive\dnd\chars"""
  let save (creature: Stats) =
    match Props.get Creature.name creature |> Option.ofObj with
    | None -> failwith "Cannot save a nameless creature"
    | Some(name) ->
      let path = Path.Combine(saveDir, name + ".txt")
      File.WriteAllText(path, JsonConvert.SerializeObject(creature, Formatting.Indented))
  let load (name: Name) =
    let path = Path.Combine(saveDir, name + ".txt")
    let json = File.ReadAllText(path)
    JsonConvert.DeserializeObject<Stats> json

module Gameplay = 
  type Act = 
    | Attacking 
    | TakingDamage
  type DamageType = Weapon | Psychic
  type Damage = Damage of damage: int * damageType: DamageType
  type Effect =
    | Hit of attacker: string * defender: string * baseRoll: int * modifier: int  * ac: int * damage:Damage
    | Miss of attacker: string * defender: string * baseRoll: int * modifier: int * ac: int 
  let rand = System.Random()
  let d x = 1 + rand.Next(x)
  let gnoll (x:int) = 
    let name = "Gnoll" + (if x = 0 then "" else x.ToString())
    let monster = 
      Creature.create name
      |> Props.set Creature.str 14
      |> Props.set Creature.dex 12
      |> Props.set Creature.con 10
      |> Props.set Creature.ac 15
    let getBonus x = (x/2) - 5
    let getStr = Lens.view Creature.str >> getBonus
    let monster =
      monster 
      |> Props.set Creature.attack (AttackDefinition.Simple "Shoot" (2 + (getStr monster)) 1 8 (getStr monster))
      |> Props.set Creature.maxHp ([for _ in 1..5 -> (d 8) + (Lens.view Creature.con monster |> getBonus)] |> List.sum)
    monster
  let dreadnought (x:int) =
    let name = "Astral Dreadought" + (if x = 0 then "" else x.ToString())
    let monster = 
      Creature.create name
      |> Props.set Creature.str 28
      |> Props.set Creature.dex 7
      |> Props.set Creature.con 25
      |> Props.set Creature.ac 20
    monster
  let team1 = List.init 120 gnoll
  let team2 = List.init 1 dreadnought
