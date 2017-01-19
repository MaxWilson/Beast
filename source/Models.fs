module Models
open System
open Util

let statBonus x = if x >= 10 then (x - 10) / 2 else -((11 - x) / 2)

type RollType = Advantage | Disadvantage | Regular
type DieRoll = { N: int; DieSize: int; Plus: int } with
    static member Create(n, d) = { N = n; DieSize = d; Plus = 0 }
    static member Create(n, d, x) = { N = n; DieSize = d; Plus = x }
    static member eval (roll: DieRoll) =
        [for x in 1..roll.N -> random (roll.DieSize) + 1] |> Seq.sum |> (+) roll.Plus
    static member eval (rolls: DieRoll list) =
        rolls |> Seq.sumBy DieRoll.eval

// The relatively invariant, combat-oriented part of a creature
type StatBlock = {
    Str: int
    Dex: int
    Con: int
    Int: int
    Wis: int
    Cha: int
    HP: int
    }
    with
    static member Create(stats: int * int * int * int * int * int, hp : int) =
        let s, d, c, i, w, ch = stats
        { Str = s; Dex = d; Con = c; Int = i; Wis = w; Cha = ch; HP = hp }
    static member Create(stats: int * int * int * int * int * int, ?hd : DieRoll) =
        let s, d, c, i, w, ch = stats
        let hp = match hd with
                 | None -> 6 + statBonus c
                 | Some(roll) -> DieRoll.eval roll
        StatBlock.Create(stats, hp)
    static member GetStr s = s.Str
    static member GetDex s = s.Dex
    static member GetCon s = s.Con
    static member GetInt s = s.Int
    static member GetWis s = s.Wis
    static member GetCha s = s.Cha
    static member GetHP s = s.HP

type Creature(stats) =
    member val Name = "Creature" with get, set

type Terrain = | Cave | Mountain | Forest | Plain

type Encounter() =
    member val Terrain = Plain with get, set
    member val Creatures : Creature[] = [||] with get, set