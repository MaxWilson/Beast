module ModelTests

open Xunit
open Util
open Models
open System
open Stat

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
    let child1 = glob.spawn()
    let child2 = glob.spawn()
    Stat.HP.Set 4 glob
    Assert.Equal(4, Stat.HP.Get child1) // Should have inherited value from global scope even though the scopes were created before global scope got value
    Assert.Equal(4, Stat.HP.Get child2)
    Stat.HP.Update 33 child1
    Stat.HP.Update 44 child2
    Assert.Equal(44, Stat.HP.Get child1) // Note: since this already existed at global scope, should update it at global scope, not child scope
    Assert.Equal(44, Stat.HP.Get child2)

[<Fact>]
let ``Imagination scopes should be able to read from but not write to real scopes``() =
    let orc = Stat.monster("Orc", (16, 12, 14, 7, 10, 8, 15))
    let customOrc = orc.spawn() // user-level customizations
    Stat.Dex.Set 14 customOrc
    Stat.Name.Set "Scro" customOrc
    let orc1 = customOrc.spawn()
    Stat.Name.Set "Leader" orc1
    Stat.HP.Set 28 orc1
    let orc2 = customOrc.spawn()
    let orc3 = customOrc.spawn()
    let orc4 = customOrc.spawn()
    Assert.Equal(14, Dex.Get orc1)
    Assert.Equal(14, Dex.Get orc2)
    Assert.Equal(14, Dex.Get orc3)
    Assert.Equal(14, Dex.Get orc4)
    Assert.Equal(28, HP.Get orc1)
    Assert.Equal(15, HP.Get orc2)
    Assert.Equal(15, HP.Get orc3)
    Assert.Equal(15, HP.Get orc4)
    Assert.Equal("Leader", Name.Get orc1)
    Assert.Equal("Scro", Name.Get orc2)
    Assert.Equal("Scro", Name.Get orc3)
    Assert.Equal("Scro", Name.Get orc4)

