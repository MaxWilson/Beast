module ModelTests

open Xunit
open Util
open Models

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
let TestStats() =
    let sb = StatBlock.Create((11, 12, 13, 14, 15, 16))
    Assert.Equal<int list>([11;12;13;14;15;16;7], [StatBlock.GetStr;StatBlock.GetDex;StatBlock.GetCon;StatBlock.GetInt;StatBlock.GetWis;StatBlock.GetCha;StatBlock.GetHP] |> List.map (flip0 sb))
