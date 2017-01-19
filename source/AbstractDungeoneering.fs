module AbstractDungeoneering

open System
open Fable.Core
open Fable.Import
module R = Fable.Helpers.React

// Check components.fs to see how to build React components from F#
open Components
open Fable.Import.React

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

type AbstractDungeon() as this =
    inherit Component<obj, ADState>(obj())
    do this.setInitState({ level = 0; xp = 0; gold = 0; items = []; log = []; isAlive = true})
    let doAdventure level=
        this.setState { this.state with xp = this.state.xp + 100; log = "Get some more XP" :: this.state.log }
        ()
    member this.render() =
        R.div [] [
            R.div [] [
                R.text [] [R.str (sprintf "You are level %d with %d XP and %d gold" this.state.level this.state.xp this.state.gold)]
                ]
            R.div[] [
                R.button [R.Props.OnClick (fun e -> doAdventure Easy)][R.str "Go on an easy adventure"]
                R.button [R.Props.OnClick (fun e -> doAdventure Vigorous)][R.str "Go on a vigorous adventure"]
                R.button [R.Props.OnClick (fun e -> doAdventure Exciting)][R.str "Go on an exciting and difficult adventure"]
                R.button [R.Props.OnClick (fun e -> doAdventure Epic)][R.str "Go on an epic and deadly adventure"]
                ]
            R.div [] (
                [R.h4 [] [R.str "Your adventures so far:"]]
                |> List.append (this.state.log |> List.map (fun entry -> R.p [] [R.text [] [R.str entry]]))
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