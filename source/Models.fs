module Models
open System
open Util

type RollType = Advantage | Disadvantage | Regular
type DieRoll = { N: int; DieSize: int; Plus: int } with
    static member Create(n, d) = { N = n; DieSize = d; Plus = 0 }
    static member Create(n, d, x) = { N = n; DieSize = d; Plus = x }
    static member eval (roll: DieRoll) =
        [for x in 1..roll.N -> random (roll.DieSize) + 1] |> Seq.sum |> (+) roll.Plus
    static member eval (rolls: DieRoll list) =
        rolls |> Seq.sumBy DieRoll.eval
