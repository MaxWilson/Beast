module ModelTests

open Xunit
open Util
open Models
open System

[<Fact>]
let CheckStatBonus() =
    Assert.Equal(+2, statBonus 14)
    Assert.Equal(+2, statBonus 15)
    Assert.Equal(-2, statBonus 7)
    Assert.Equal(-1, statBonus 8)
    Assert.Equal(-1, statBonus 9)
    Assert.Equal(0, statBonus 10)
    Assert.Equal(0, statBonus 11)
    Assert.Equal(+10, statBonus 30)

[<Fact>]
let TestFlip() =
    Assert.Equal<int list>([1;2;3], [4;5;6] |> List.map (flip (-) 3))

[<Fact>]
let TestScopes() =
    let glob = Stat.StatScope()
    Assert.Equal(0, Stat.HP.Get glob) // should default to 0
    let child1 = glob.spawn()
    let child2 = glob.spawn()
    Assert.True(Assert.Throws<Exception>(fun () -> Stat.Name.Update "Bob" child1).Message.Contains("found no value for 'Name'"))
    Assert.Equal(0, Stat.HP.Get child1) // should default to 0
    Assert.Equal(0, Stat.HP.Get child2)
    Stat.HP.Set 77 child1
    Stat.HP.Set 55 child2
    Assert.Equal(77, Stat.HP.Get child1)
    Assert.Equal(55, Stat.HP.Get child2)
    Stat.HP.Update 33 child1
    Stat.HP.Update 44 child2
    Assert.Equal(33, Stat.HP.Get child1)
    Assert.Equal(44, Stat.HP.Get child2)
    Stat.HP.Set 4 glob
    let child1 = glob.spawn()
    let child2 = glob.spawn()
    Assert.Equal(4, Stat.HP.Get child1) // Should have inherited value from global scope
    Assert.Equal(4, Stat.HP.Get child2)
    Stat.HP.Update 33 child1
    Stat.HP.Update 44 child2
    Assert.Equal(44, Stat.HP.Get child1) // Note: since this already existed at global scope, should update it at global scope, not child scope
    Assert.Equal(44, Stat.HP.Get child2)
