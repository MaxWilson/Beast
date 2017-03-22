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

module Maze =
  // implicit data structure storing edges in the maze--no need to track the cells themselves
  type data = bool[][] * bool[][]
  let create (n:int) : data =
    (Array.init (n-1) (fun _ -> Array.create n false)), (Array.init (n-1) (fun _ -> Array.create n false))
  let create2D (n:int) (m:int) : data =
    (Array.init (n-1) (fun _ -> Array.create m false)), (Array.init (m-1) (fun _ -> Array.create n false))
  let carveLeft (d:data) (x,y) =
    if x > 0 then
      (fst d).[x-1].[y] <- true
  let leftOpen (d:data) (x,y) =
    x > 0 && (fst d).[x-1].[y]
  let carveRight (d:data) (x,y) =
    if x < (fst d).Length then
      (fst d).[x].[y] <- true
  let rightOpen (d:data) (x,y) =
    x < (fst d).Length && (fst d).[x].[y]
  let carveUp (d:data) (x,y) =
    if y > 0 then
      (snd d).[y-1].[x] <- true
  let upOpen (d:data) (x,y) =
    y > 0 && (snd d).[y-1].[x]
  let carveDown (d:data) (x,y) =
    if y < (snd d).Length then
      (snd d).[y].[x] <- true
  let downOpen (d:data) (x,y) =
    y < (snd d).Length && (snd d).[y].[x]
  /// return the actual dimensions of the maze
  let dimensions (d:data) = (fst d).Length + 1, (snd d).Length + 1
