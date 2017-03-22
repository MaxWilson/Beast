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
  type data = bool[,] * bool[,]
  let create (n:int) : data =
    (Array2D.create (n-1) n false), (Array2D.create n (n-1) false)
  let carveLeft (d:data) (x,y) =
    if x > 0 then
      (fst d).[x-1,y] <- true
  let leftOpen (d:data) (x,y) =
    x > 0 && (fst d).[x-1,y]
  let carveRight (d:data) (x,y) =
    if x < (fst d).GetLength(0) then
      (fst d).[x,y] <- true
  let rightOpen (d:data) (x,y) =
    x < (fst d).GetLength(0) && (fst d).[x,y]
  let carveUp (d:data) (x,y) =
    if y > 0 then
      (snd d).[x,y-1] <- true
  let upOpen (d:data) (x,y) =
    y > 0 && (snd d).[x,y-1]
  let carveDown (d:data) (x,y) =
    if y < (snd d).GetLength(1) then
      (snd d).[x,y] <- true
  let downOpen (d:data) (x,y) =
    y < (snd d).GetLength(1) && (snd d).[x,y]
