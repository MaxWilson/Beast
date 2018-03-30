open System
open System.Collections.Generic
type Stats = Map<string, obj>
type Name = string
#r "Newtonsoft.Json.dll"
// Lens code based on http://www.fssnip.net/7Pk/title/Polymorphic-lenses by Vesa Karvonen

type Lens<'s,'t,'a,'b> = ('a -> Option<'b>) -> 's -> Option<'t>
module Lens =
  let view l s =
    let r = ref Unchecked.defaultof<_>
    s |> l (fun a -> r := a; None) |> ignore
    !r

  let over l f =
    l (f >> Some) >> function Some t -> t | _ -> failwith "Impossible"
  let set l b = over l <| fun _ -> b
  let lens get set : Lens<_,_,_,_> = fun f s ->
    (get s |> f : Option<_>) |> Option.map (fun f -> set f s)

module Props =
  let prop<'t> (propName: string) f =
    Lens.lens
      (fun x -> match Map.tryFind propName x with | Some(:? 't as v:obj) -> (v |> unbox<'t>) | Some v -> failwithf "Could not convert %A to %s" v (typeof<'t>.Name) | None -> Unchecked.defaultof<'t>)
      (fun v x -> Map.add propName v x)
      f
  let singleList f =
    Lens.lens
      (List.head)
      (fun v x -> [v])
      f
  let set prop v stats = stats |> Lens.set prop (box v)
  let get prop stats = stats |> Lens.view prop

module DataTypes =
  type DieRoll = DieRoll of number: int * dieSize: int | StaticModifier of int
  type DamageType = Weapon of magical: bool | Fire | Poison
  type DamageDefinition = { damageType: DamageType; amount: DieRoll list }
  type AttackDefinition = { name: string; toHit: int; damages: DamageDefinition list }
    with
    static member simple name toHit n d plus =
      { name = name; toHit = toHit; damages = [{ damageType = Weapon(false); amount = if plus <> 0 then [DieRoll(n, d); StaticModifier(plus)] else [DieRoll(n,d)] }] }

module Creature =
  open Props
  open DataTypes
  let name = prop<Name> "Name"
  let hp = prop<int64> "HP"
  let maxHp = prop<int64> "MaxHP"
  let attacks = prop<AttackDefinition list> "Attacks"
  let attack = attacks >> singleList
  let create name' =
    Map.empty |> set name name'

module Menagerie =
  open System.IO
  open Newtonsoft.Json
  let mutable saveDir = """C:\Users\maxw\OneDrive\dnd\chars"""
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

Menagerie.save (Creature.create "Shawn" |> Props.set Creature.hp 77 |> Props.set Creature.attack (DataTypes.AttackDefinition.simple "Bite" +4 2 8 +2))
Menagerie.load "Shawn" |> Menagerie.save

let shawn = Menagerie.load "Shawn"
shawn |> Props.get Creature.maxHp