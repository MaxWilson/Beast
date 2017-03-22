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
    currentPosition: int * int
    revealed: (int * int) list
    eventGen: (int * int) -> string option
    messages: (int*int*string) list
  }

let binaryMaze width height ()=
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

module List =
  let every pred = List.exists (not << pred) >> not

let eventsFor (maze: Maze.data) n =
  let r = new Random()
  let table = [|
    "Goblins attack!"
    "Rats attack!"
    "You see a golden treasure (50 gp)."
    "You feel suddenly weak. Make a DC 10 Con save or take 2d6 necrotic damage."
    "Zombies attack!"
    "The floor gives way and you fall in a pit. Take 1d6 falling damage"
    |]
  let mutable events = [
    for i in 1..n do
      let xmax, ymax = Maze.dimensions maze
      let coords = r.Next(xmax), r.Next(ymax)
      yield coords, table.[r.Next(table.Length)]
    ]
  let lineOfSight (x,y) (m,n) : bool =
    if (x,y) = (m,n) then
      true
    elif x = m then // same vertical line--check if there is vertical line of sight
      let start, finish = min n y, max n y
      [start..finish-1] |> List.every(fun y -> Maze.downOpen maze (x,y))
    elif y = n then // same horizontal line--check if there is horizontal line of sight
      let start, finish = min m x, max m x
      [start..finish-1] |> List.every(fun x -> Maze.rightOpen maze (x,y))
    else // there are no diagonal lines of sight
      false
  fun coords ->
    // find anything in the direct line of sight of the current location
    match events |> List.tryFind (fst >> lineOfSight coords) with
    | None ->
      None
    | Some(triggeredEvent) ->

      // remove triggered events from list so they don't happen twice
      events <- events |> List.filter ((<>)triggeredEvent)
      printfn "Remaining events at: %A" (events |> List.map fst)

      // Return the event that occurred
      Some(snd triggeredEvent)

type Direction =
  | Left
  | Right
  | Up
  | Down

type Msg =
  | Refresh
  | Move of Direction

let init _ =
  { maze = None; mazeGenerator = binaryMaze 22 10; currentPosition = (0,0); messages = []; revealed = []; eventGen = (fun _ -> None) }, [fun d -> d Refresh]

let update msg model =
  match msg with
  | Refresh ->
    let maze = model.mazeGenerator()
    let xdim, ydim = Maze.dimensions maze
    { model with maze = Some(maze); currentPosition = (0,0); messages = []; revealed = []; eventGen = eventsFor maze (xdim * ydim / 8) }, []
  | Move(dir) ->
    let boundsCheck maze (x, y) =
      let w, h = Maze.dimensions maze
      let between low high v = low <= v && v < high
      between 0 w x && between 0 h y
    let moveCheck =
      match dir with
      | Left -> Maze.leftOpen
      | Right -> Maze.rightOpen
      | Up -> Maze.upOpen
      | Down -> Maze.downOpen
    let x, y =
      match dir, model.currentPosition with
      | Left, (x,y) -> x-1, y
      | Right, (x,y) -> x+1, y
      | Up, (x,y) -> x,y-1
      | Down, (x,y) -> x,y+1
    match model.maze with
    | Some(maze) when boundsCheck maze (x,y) && moveCheck maze model.currentPosition ->
      match model.eventGen (x,y) with
      | None ->
        { model with currentPosition = (x,y) }, []
      | Some(message) ->
        { model with currentPosition = (x,y); messages = (x,y,message)::model.messages }, []
    | _ ->
      model, []

module Key =
  let left = KeyDetect 37
  let up = KeyDetect 38
  let right = KeyDetect 39
  let down = KeyDetect 40

let view (model: ViewModel) dispatch =
  Key.left.Pressed <- delay dispatch (Move Left)
  Key.right.Pressed <- delay dispatch (Move Right)
  Key.up.Pressed <- delay dispatch (Move Up)
  Key.down.Pressed <- delay dispatch (Move Down)

  R.div [ClassName "shell"] [
    R.div [] [
      R.button [OnClick (fun _ -> dispatch Refresh)] [R.str "Refresh"]
      R.text [] [
          match model.messages |> List.filter (fun (x,y,msg) -> (x,y) = model.currentPosition) with
          | [] -> "Nothing interesting here"
          | msgs -> String.Join("; also ", msgs |> List.map(fun(_,_,msg) -> msg))
          |> R.str
        ]
      ]
    PixiBox.Create(fun(w,h) ->
      let stage = new Container()
      let g = stage.addChild(new Graphics()) :?> Graphics
      match model.maze with
      | Some(maze) ->
        let xlen, ylen = Maze.dimensions maze
        let cellHeight = h/float ylen
        let cellWidth = w/float xlen
        let toX x = float x * cellWidth
        let toY y = float y * cellHeight
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
        // Also, draw the current position in red
        let left, top =
          match model.currentPosition with
          | (x,y) -> toX x, toY y
        g.lineStyle(0.).beginFill(float 0x891121)
          .drawRect(left + 10., top + 10., cellWidth - 20., cellHeight - 20.) // leave 10-pixel margin because it looks good
          .endFill() |> ignore
        for (x,y,msg) in model.messages do
          let elide maxLen (msg:string) =
            if msg.Length > maxLen then msg.Substring(0, maxLen) + "..."
            else msg
          stage.addChild(Text(elide 16 msg, [TextStyle.WordWrap true; TextStyle.WordWrapWidth cellWidth], position=Point(toX x, toY y))) |> ignore
      | None -> ()
      stage
      )
    ]


Program.mkProgram init update view
 //|> Program.toNavigable
 |> Program.withReact "content"
 |> Program.run