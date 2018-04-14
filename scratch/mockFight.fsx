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
  let prop<'t> (propName: string) f =
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
  let propWithFallback<'t> (propName: string) fallbackLens f =
    propBase<'t> propName (get fallbackLens) f

module DataTypes =
  type DieRoll = DieRoll of number: int * dieSize: int | StaticModifier of int
  type DamageType = Weapon of magical: bool | Fire | Poison
  type DamageDefinition = { damageType: DamageType; amount: DieRoll list }
  type AttackDefinition = { name: string; toHit: int; damages: DamageDefinition list }
    with
    static member Simple name toHit n d plus =
      { name = name; toHit = toHit; damages = [{ damageType = Weapon(false); amount = if plus <> 0 then [DieRoll(n, d); StaticModifier(plus)] else [DieRoll(n,d)] }] }

module Creature =
  open Props
  open DataTypes
  let name = prop<Name> "Name"
  let maxHp = prop<int> "MaxHP"
  let hp = propWithFallback<int> "HP" maxHp
  let attacks = propWithFallback<AttackDefinition list> "Attacks" (Lens.constant [])
  let attack = singleList >> attacks
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

