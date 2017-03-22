module dunGen

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
open Components
open PIXI

type ViewModel = {
    maze: Maze.data
  }
let init _ = (), []
let update model msg = (), []
let view () dispatch =
  PixiBox.Create(fun(w,h) ->
    let g = new Graphics()
    g.lineStyle(3.) |> ignore
    for x in 0. .. 10. ..w do
      g.moveTo(x, 0.)
        .lineTo(x, h)
        |> ignore
    for y in 0. .. 10. ..h do
      g.moveTo(0., y)
        .lineTo(w, y)
        |> ignore
    g
    )

Program.mkProgram init update view
 //|> Program.toNavigable
 |> Program.withReact "content"
 |> Program.run