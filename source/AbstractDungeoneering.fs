module AbstractDungeoneering

open System
open Fable.Core
open Fable.Import
module R = Fable.Helpers.React

// Check components.fs to see how to build React components from F#
open Components
open Fable.Import.React
open Util

// Polyfill for ES6 features in old browsers
Node.require.Invoke("core-js") |> ignore


[<Pojo>]
type ADProps =
    { x: unit }
[<Pojo>]
type ADState =
    {
        isAlive: bool
        level: int
        xp: int
        gold: int
        items: string list
        log: string list
    }

type Difficulty = Easy | Vigorous | Exciting | Epic
type Effect = Gold of int | XP of int | LoseGold | Item of string
let level targetLevel actualLevel =
    if targetLevel < actualLevel then
        false // not killed
    elif targetLevel > actualLevel + 5 then // waaaay underlevel
        (random 100) <= (targetLevel - actualLevel) * 10 // 10% chance of death per level under
    else // somewhat underlevel or on target
        (random 100) <= (targetLevel - actualLevel + 1) * 5 // 5% chance of death per level under
type TableRow = { range: int * int; fatality: (int -> bool); description : string; effects: Effect list }
type Table = TableRow list
let setCounters = (List.mapFold (fun count (n, diff, descr, effects) -> { range = count + 1, count + n; fatality = diff; description = descr; effects = effects }, n + count) 0) >> fst
let easyTable : Table =
    setCounters [
        4, level 1, "Patrol for goblins", [Gold 10; XP 100]
        4, level 1, "Patrol for zombies", [Gold 5; XP 100]
        4, level 2, "Patrol for a band of goblins", [Gold 20; XP 200]
        2, level 3, "Patrol for orcs", [Gold 50; XP 300]
        1, level 0, "Help a starving widow find food", [XP 300]
    ]
let vigorousTable : Table =
    setCounters [
        5, level 4, "Clean out a meenlock infestation", [XP 500]
        3, level 5, "Loot a wight's tomb", [Gold 500; XP 1000; Item "Silver mace"]
        2, level 5, "Loot a wight's tomb", [Gold 500; XP 1000; Item "Jade Dagger +1"]
        3, level 6, "Slay a mummy", [Gold 5000; XP 1000; Item "Mail shirt +1"]
        1, level 5, "Raise a dragon hatchling from an egg", [XP 500; Item "Dragon wyrmling"]
        10, level 5, "Escort a merchant safely through pirate-infested waters", [Gold 1000; XP 300]
    ]
let excitingTable : Table =
    setCounters [
        1, level 9, "Fight off a githyanki war-band", [Gold 4000; XP 9000; Item "Potion of dragon control"]
        5, level 8, "Solve a murder (vampires did it) and apprehend the murderer", [Gold 1000; XP 8000]
        2, level 7, "Save the king from doppelganger kidnapping", [Gold 800; XP 4000; Item "Ancestral longsword +1" ]
        10, level 6, "Repel small orcish invasion", [Gold 1000; XP 4000]
        1, level 1, "A thief has stolen your riches!", [LoseGold]
    ]
let epicTable : Table =
    setCounters [
        3, level 17, "Slay huge red dragon and claim its hoard", [Gold 50000; XP 25000; Item "Wand of Web"; Item "Plate Armor of Fire Resistance +1"]
        3, level 21, "Slay huge red dragon and claim its hoard", [Gold 50000; XP 25000; Item "Staff of the Magi"]
        20, level 16, "Repel massive orcish invasion", [Gold 8000; XP 15000]
        4, level 17, "Fight off Githyanki war-band led by knights", [Gold 20000; XP 25000; Item "Silver Greatsword of Gith +3"]
        1, level 25, "Slay legendary red dragon and claim its hoard", [Gold 200000; XP 50000; Item "Stormcleaver artifact"; Item "Plate armor of invulnerability"]
    ]

let recomputeLevel =
    let levelMins = [
        0
        300
        900
        2700
        6500
        14000
        23000
        34000
        48000
        64000
        85000
        100000
        120000
        140000
        165000
        195000
        225000
        265000
        305000
        355000
    ]
    fun xp ->
        levelMins |> List.findIndexBack (flip (<=) xp) |> (+) 1
let rollOn (table: Table) (state: ADState) =
    if not state.isAlive then
        state
    else
        let lastCount = (table |> List.last).range |> snd
        let roll = random lastCount
        let adventure = table |> List.find (fun { range = (lower, upper) } -> lower <= roll && roll <= upper)
        let state = if adventure.fatality state.level then
                        { state with isAlive = false; log = adventure.description + " (killed)" :: state.log }
                    else
                        let applyEffects (state : ADState) = function
                            | Gold(x) -> { state with gold = state.gold + x }
                            | XP(x) -> { state with xp = state.xp + x }
                            | LoseGold -> { state with gold = 0 }
                            | Item item -> { state with items = item :: state.items }
                        adventure.effects |> List.fold applyEffects { state with log = adventure.description :: state.log }
        let state = { state with level = recomputeLevel state.xp }
        state

type AbstractDungeon() as this =
    inherit Component<obj, ADState>(obj())
    let init = { level = 1; xp = 0; gold = 0; items = []; log = []; isAlive = true}
    do this.setInitState(init)
    let doAdventure level =
        let t = match level with | Easy -> easyTable | Vigorous -> vigorousTable | Exciting -> excitingTable | Epic -> epicTable
        this.setState (rollOn t this.state)
    member this.render() =
        let descr = (sprintf "%sYou are level %d with %d XP and %d gold" (if this.state.isAlive then "" else "(Dead) ") this.state.level this.state.xp this.state.gold)
        let descr = if this.state.items.IsEmpty then descr
                    else System.String.Join(" and ", descr :: this.state.items)
        R.div [] [
            R.div [] [
                R.text [] [R.str descr]
                ]
            R.div[] [
                R.button [R.Props.OnClick (fun e -> doAdventure Easy)][R.str "Go on an easy adventure"]
                R.button [R.Props.OnClick (fun e -> doAdventure Vigorous)][R.str "Go on a vigorous adventure"]
                R.button [R.Props.OnClick (fun e -> doAdventure Exciting)][R.str "Go on an exciting and difficult adventure"]
                R.button [R.Props.OnClick (fun e -> doAdventure Epic)][R.str "Go on an epic and deadly adventure"]
                R.button [R.Props.OnClick (fun e -> this.setState init)][R.str "Reset (new character)"]
                ]
            R.div [] (
                R.h4 [] [R.str "Your adventures so far:"] :: (this.state.log |> List.map (fun entry -> R.p [] [R.text [] [R.str entry]]))
                )
            ]

ReactDom.render(
    R.div [] [
        R.h1 [] [R.str "Abstract Dungeoneering"]
        R.h3 [] [R.str "Advanced character creation"]
        R.com<AbstractDungeon, _, _>() []
        ],
    Browser.document.getElementById "content")
|> ignore