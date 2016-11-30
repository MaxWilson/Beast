#r @"..\bin\Fable.Core.dll"
#r @"..\bin\Fable.React.dll"
#load @"..\source\Models.fs"
#load @"..\source\Components.fs"
open Models
let x = Encounter(Creatures=[|Creature(Name="Bob")|])
printfn "%A" x