module Beast

open System
open Fable.Core
open Fable.Import
module R = Fable.Helpers.React

// Check components.fs to see how to build React components from F#
open Components
open Fable.Import.React

// Polyfill for ES6 features in old browsers
Node.require.Invoke("core-js") |> ignore
Node.require.Invoke("../css/app.css") |> ignore

open Util
open Models
open System
open Stat
open Elmish.Browser.Navigation
open Elmish.React
open Elmish
open Fable.Import.Browser
open Elmish.UrlParser
open Fable.Import.React
open Fable.Import.ReactDom
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.PIXI

module ViewModel =
  type EncounterType = ShortRest | LongRest | Fight
  type EncounterBuildState = {
    currentMonster: StatScope option
    roster: Roster
    }
  type RunAdventureState = {
    party: Party
    encounters: Encounter list
    }
  type ViewModelState =
    | EncounterBuilder of EncounterBuildState
    | RunAdventure of RunAdventureState
  type ViewModel = {
    model: Model
    }
open ViewModel

type Msg = | SetField of string * string
let init _ = Model.create(), []
let update msg (model:Model) =
  match msg with
  | SetField(name, value) ->
    
    { model with }
  | NewString(txt) ->
    { model with message = txt }, [fun dispatch -> dispatch Increment]
  | Increment -> { model with count = model.count + 1 }, []

let view (model:ViewModel) dispatch =
  R.div [ClassName "shell"] [
    PixiBox.Create (fun (w:Width, h:Height) ->
      let stage = Container()
      for i in 1..model.count do
        let txt = Text(
                      model.message, [
                        FontFamily "Arial"
                        FontSize "24px"
                        FontStyle "italic"
                        FontWeight "bold"
                        Fill (U2.Case1 "lightgrey")
                        Align "center"
                        Stroke (U2.Case1 "darkgrey")
                        StrokeThickness 7.
                        ],
                      position = Point(Util.randomRational(w), Util.randomRational(h))
                      )
        stage.addChild(txt) |> ignore
      stage.addChild(Text(model.count.ToString(),
                          [
                            FontFamily "Arial"
                            FontSize "24px"
                            FontStyle "italic"
                            FontWeight "bold"
                            Fill (U2.Case1 "lightgrey")
                            Align "center"
                            Stroke (U2.Case1 "darkgrey")
                            StrokeThickness 7.
                            ])) |> ignore
      stage
    )
    R.div [] [
      R.text [] [R.str (model.message)]
      R.br [] []
      R.button [OnClick (fun _ -> dispatch (NewString (if model.message = "Hello" then "Goodbye" else "Hello")))] [R.str "Toggle"]
      ]
    ]

Program.mkProgram init update view
 //|> Program.toNavigable
 |> Program.withReact "content"
 |> Program.run