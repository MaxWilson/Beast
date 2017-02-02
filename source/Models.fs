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

module Stat =
    open System.Collections.Generic

    type StatScope private (parentScope: StatScope option) =
        let vals = Dictionary<string, obj>()
        new() = StatScope(None)
        member this.get<'a>(key:string): 'a option =
            match vals.TryGetValue key with
            | true, v -> Some(unbox v)
            | _ ->
                if parentScope.IsSome then
                    parentScope.Value.get key
                else
                    None
        member this.update<'a>(key, (valueMaker: 'a -> 'a)) =
            match vals.TryGetValue key with
            | true, v ->
                vals.[key] <- valueMaker(unbox v)
            | _ ->
                if parentScope.IsSome then
                    parentScope.Value.update(key, valueMaker)
                else
                    failwithf "Unable to update: found no value for '%s' to update" key
        member this.set key value =
            vals.[key] <- value
        member this.spawn() =
            StatScope(Some this)
        member this.imagine(?realParent) =
            ()

    type Property<'a> = { Name: string; Get: StatScope -> 'a; Update: 'a -> StatScope -> unit; Set: 'a -> StatScope -> unit }
    let valueProp<'a> name defaultVal =
        let getVal (scope: StatScope) = match scope.get<'a> name with | Some(v) -> v | None -> defaultVal
        let updateVal (v:'a) (scope: StatScope) = scope.update<'a>(name, (fun _ -> v))
        let setVal (v:'a) (scope: StatScope) = scope.set<'a> name v
        { Name = name; Get = getVal; Update = updateVal; Set = setVal }

    let intProp = valueProp<int>
    let stringProp = valueProp<string>

    let HP = intProp "HP" 0
    let Name = stringProp "Name" "Nameless"
    let Str = intProp "Str" 10
    let Dex = intProp "Dex" 10
    let Con = intProp "Con" 10
    let Int = intProp "Int" 10
    let Wis = intProp "Wis" 10
    let Cha = intProp "Cha" 10

    let monster(name, (str, dex, con, int, wis, cha, hp)) =
        let sc = StatScope()
        [
            Str.Set str
            Dex.Set dex
            Con.Set con
            Int.Set int
            Wis.Set wis
            Cha.Set cha
            HP.Set hp
            Name.Set name
        ] |> List.iter (apply sc)
        sc