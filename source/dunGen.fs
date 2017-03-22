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
    maze: Maze.data option
    mazeGenerator: unit -> Maze.data
  }

let binaryMaze() =
  let width = 22
  let height = 10
  let m = Maze.create2D width height
  let r = new Random()
  for y in 0 .. (height-1) do
    for x in 0 .. (width-1) do
      // choose randomly, except that bottom row must go right and right row must go down
      if (x+1 = width || r.Next(2) = 0) && (y+1 <> height) then
        Maze.carveDown m (x,y)
      else
        Maze.carveRight m (x,y)
  m

type Msg =
  | Refresh
let init _ =
  { maze = None; mazeGenerator = binaryMaze }, [fun d -> d Refresh]
let update msg model =
  match msg with
  | Refresh ->
    { model with maze = Some(model.mazeGenerator()) }, []
let view (model: ViewModel) dispatch =
  R.div [ClassName "shell"] [
    R.button [OnClick (fun _ -> dispatch Refresh)] [R.str "Refresh"]
    PixiBox.Create(fun(w,h) ->
      let g = new Graphics()
      match model.maze with
      | Some(maze) ->
        let xlen, ylen = Maze.dimensions maze
        let cellHeight = h/float ylen
        let cellWidth = w/float xlen
        g.lineStyle(5., float 0x696969 ) |> ignore
        g.moveTo(0.,2.).lineTo(w,2.).moveTo(2.,0.).lineTo(2.,h) |> ignore
        g.moveTo(0.,h-2.).lineTo(w,h-2.).moveTo(w-2.,0.).lineTo(w-2.,h) |> ignore
        for x in 0 .. xlen - 1 do
          for y in 0.. ylen - 1 do
            let left = (float x) * cellWidth
            let right = (float x + 1.) * cellWidth
            let top = (float y) * cellHeight
            let bottom = (float y + 1.) * cellHeight
            if not <| Maze.rightOpen maze (x,y) then
              g.moveTo(right, top).lineTo(right, bottom) |> ignore
            if not <| Maze.downOpen maze (x,y) then
              g.moveTo(left, bottom).lineTo(right, bottom) |> ignore
      | None -> ()
      g
      )
    ]

Program.mkProgram init update view
 //|> Program.toNavigable
 |> Program.withReact "content"
 |> Program.run