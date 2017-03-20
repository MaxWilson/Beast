module Models
open System
open Util
open Stat

type RollType = Advantage | Disadvantage | Regular
type DieRoll = { N: int; DieSize: int; Plus: int } with
    static member Create(n, d) = { N = n; DieSize = d; Plus = 0 }
    static member Create(n, d, x) = { N = n; DieSize = d; Plus = x }
    static member eval (roll: DieRoll) =
        [for x in 1..roll.N -> random (roll.DieSize) + 1] |> Seq.sum |> (+) roll.Plus
    static member eval (rolls: DieRoll list) =
        rolls |> Seq.sumBy DieRoll.eval

type Roster = StatScope list
type Bestiary = Map<string, StatScope>
type Party = Roster
type Encounter = Roster
type Adventure = ShortRest | LongRest | Encounter of Encounter
type Model = {
  bestiary: Bestiary
  party: Party
  adventure: Adventure
  }
  with static member create() = { bestiary = Map.empty; party = []; adventure = [] }

module DM =
  open Fable.Import.JS
  open Fable.PowerPack

  let runCombat (m:Model) : Promise<Model> =
    promise { return m } // stub