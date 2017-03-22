module ModelTests

open Xunit
open Util
open Models
open System
open Stat
open dunGen

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
    let orcs = [orc1;orc2;orc3;orc4]
    Assert.Equal<int list>([14;14;14;14], orcs |> List.map Dex.Get)
    Assert.Equal<int list>([28;15;15;15], orcs |> List.map HP.Get)
    Assert.Equal<string list>(["Leader";"Scro";"Scro";"Scro"], orcs |> List.map Name.Get)
    let imagined = orcs |> Stat.imagine
    Stat.HP.Update 14 (imagined.[0])
    Stat.Name.Update "Dugtash" (imagined.[1]) // imagine that the second orc's name is Dugtash
    // check that changing imagination things works but has no impact on real things
    Assert.Equal<int list>([14;15;15;15], imagined |> List.ofArray |> List.map HP.Get)
    Assert.Equal<string list>(["Leader";"Dugtash";"Scro";"Scro"], imagined |> List.ofArray |> List.map Name.Get)
    // but in the real world, HP and names haven't changed
    Assert.Equal<int list>([28;15;15;15], orcs |> List.map HP.Get)
    Assert.Equal<string list>(["Leader";"Scro";"Scro";"Scro"], orcs |> List.map Name.Get)
    let nestedImagination = imagined |> Stat.imagine
    // check that a nested imagination can still get values from the real world, and set values without disturbing our parent imagination
    Stat.HP.Update 3 (nestedImagination.[1])
    Assert.Equal<int list>([14;3;15;15], nestedImagination |> List.ofArray |> List.map Stat.HP.Get)
    Assert.Equal<string list>(["Leader";"Dugtash";"Scro";"Scro"], nestedImagination |> List.ofArray |> List.map Name.Get)
    Assert.Equal<int list>([28;15;15;15], orcs |> List.map HP.Get)
    Assert.Equal<string list>(["Leader";"Scro";"Scro";"Scro"], orcs |> List.map Name.Get)

[<Fact>]
let ``Carved passages through a maze should be bidirectional and bounds-checked``() =
  let d = Maze.create 2
  Maze.carveLeft d (0,0) // should do nothing--cannot exit the maze
  Maze.carveRight d (0,0) // should create bidirectional passage between 0,0 and 1,0
  Maze.carveDown d (1,0) // should create bidirectional passage between 1,0 and 1,1
  let openings coords = Maze.leftOpen d coords, Maze.rightOpen d coords, Maze.upOpen d coords, Maze.downOpen d coords
  Assert.Equal((false, true, false, false), openings (0,0))
  Assert.Equal((true, false, false, true), openings (1,0))
  Assert.Equal((false, false, true, false), openings (1,1))
  Assert.Equal((false, false, false, false), openings (0,1))
