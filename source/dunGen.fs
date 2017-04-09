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
open System.Runtime.CompilerServices
open Fable.Core.JsInterop
open Models.Maze

type MazeGenerator = unit -> Maze.data
type Algorithm =
  | Binary
  | HunterKiller
  | ABW
  with
  static member All = [Binary; HunterKiller; ABW]
  static member Render =
    function
    | Binary -> "Binary"
    | HunterKiller -> "Hunter-Killer"
    | ABW -> "Aldous-Broder/Wilson's"
  static member Parse str =
    Algorithm.All |> List.find (fun alg -> Algorithm.Render alg = str)

type ViewModel = {
    maze: Maze.data option
    mazeGenerator: MazeGenerator
    algorithm: Algorithm
    currentPosition: int * int
    revealed: (int * int) Set
    eventGen: (int * int) -> string option
    messages: (int*int*string) list
    goal: int * int
    points: int
    steps: int
  }

let rec newReveals maze direction position =
  if moveCheck direction maze position then
    position::(newReveals maze direction (move direction position))
  else [position]

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

let huntKill width height ()=
  let maze = Maze.create2D width height
  let r = new Random()
  let mutable visited = Set.empty
  let boundsCheck (x,y) =
    0 <= x && x < width && 0 <= y && y < height
  let rec kill currentNode =
    visited <- visited.Add(currentNode)
    // don't revisit any already-visited nodes, and don't go out of bounds
    match Direction.All |> List.filter (fun d ->
      let node' = move d currentNode
      boundsCheck node' && (not <| visited.Contains(node'))
      ) with
    | [] -> hunt currentNode
    | lst ->
      let dir = lst.[r.Next(lst.Length)]
      let nextNode = move dir currentNode
      Maze.carveDirection dir maze currentNode
      // continue killing
      kill nextNode
  and hunt currentNode =
    let candidates = seq {
      // Scan until we find an unvisited node that is adjacent to a visited node
      for x in 0..width-1 do
        for y in 0..height-1 do
          let node = (x,y)
          if not (visited.Contains(node)) then
            for d in Direction.All do
              let node' = Maze.move d node
              if boundsCheck node' && visited.Contains(node') then
                yield (node, d)
      }
    match candidates |> Seq.tryHead with
    | Some(node, d) ->
      Maze.carveDirection d maze node // connect the two nodes
      kill node // resume kill
    | None ->
      () // done with maze
  kill (0,0)
  maze

let aldousBroderWilsons width height ()=
  // We actually only implement Aldous-Broder instead of a hybrid,
  // because perf turns out to be good enough. But it produces the same
  // maze as Wilson's or a hybrid would, so I'll leave the label
  // "Aldous-Broder/Wilson's" intact.
  let maze = Maze.create2D width height
  let r = new Random()
  let mutable visited = Set.empty
  let mutable unvisited = Set.ofList (List.cross [0..width-1] [0..height-1])
  let visit node =
    visited <- visited.Add(node)
    unvisited <- unvisited.Remove(node)
  let boundsCheck (x,y) =
    0 <= x && x < width && 0 <= y && y < height
  let mutable node = (0, 0)
  while unvisited.Count > 0 do
    visit node
    // choose a direction that doesn't take us outside the maze
    let validDirections = Direction.All |> List.filter (fun d -> boundsCheck (move d node))
    let dir = validDirections.[r.Next(validDirections.Length)]
    let node' = (move dir node)
    if unvisited.Contains(node') then
      // connect the two nodes
      Maze.carveDirection dir maze node
    node <- node'
  maze

let eventsFor (maze: Maze.data) n =
  let r = new Random()
  let table = [|
    "Goblins attack!"
    "Rats attack!"
    "You see a golden treasure (50 gp)."
    "You feel suddenly weak. Make a DC 10 Con save or take 2d6 necrotic damage."
    "Zombies attack!"
    "The floor gives way and you fall in a pit. Take 1d6 falling damage"
    "Mutants attack!"
    "A Pit Fiend attacks!"
    "A Blue Dragon attacks!"
    "Carrion crawler attacks!"
    "You find a body, a suit of chain mail, and 500 gp."
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

type Msg =
  | Refresh
  | Reveal
  | Move of Direction
  | SwitchAlgorithm of Algorithm

let init _ =
  { maze = None; mazeGenerator = (fun () -> Maze.create 1); algorithm = Algorithm.All |> List.last; currentPosition = (0,0); messages = []; revealed = Set.empty; eventGen = (fun _ -> None); goal = (0,0); points = 0; steps = 0 }, [(fun d -> d (SwitchAlgorithm (Algorithm.All |> List.last))); (fun d -> d Refresh)]

let update msg model =
  match msg with
  | Refresh ->
    let maze = model.mazeGenerator()
    let xdim, ydim = Maze.dimensions maze
    let revealed = [Left;Right;Up;Down] |> List.collect (fun d -> newReveals maze d (0,0)) |> Set.ofList
    let r = new Random()
    let goal =
      match (r.Next(xdim), r.Next(ydim)) with
      | (0,0) -> (xdim-1, ydim-1) // never want goal in start position, so just pick opposite corner in this rare case. Could recur instead but why bother?
      | x,y -> x,y
    { model with maze = Some(maze); currentPosition = (0,0); messages = []; revealed = revealed; eventGen = eventsFor maze (xdim * ydim / 5); goal = goal; steps = 0 }, []
  | Move(dir) ->
    let boundsCheck maze (x, y) =
      let w, h = Maze.dimensions maze
      let between low high v = low <= v && v < high
      between 0 w x && between 0 h y
    let x, y = move dir model.currentPosition
    match model.maze with
    | Some(maze) when boundsCheck maze (x,y) && moveCheck dir maze model.currentPosition ->
      let newRevealed = [Left;Right;Up;Down] |> List.collect (fun d -> newReveals maze d (x,y))
      let model' = { model with currentPosition = (x,y); revealed = model.revealed |> Set.union(Set.ofList newRevealed); steps = model.steps + 1 }
      let model' =
        match model'.eventGen (x,y) with
        | None ->
          model'
        | Some(message) ->
          { model' with messages = (x,y,message)::model.messages }
      // check for goal and if so then refresh
      if model'.goal = model'.currentPosition then
        { model' with points = model.points + (max 0 ((200 - model.steps) * 10)) }, [fun d -> d Refresh]
      else
        model', []
    | _ ->
      model, []
  | Reveal ->
    match model.maze with
    | Some(maze) ->
       let xdim, ydim = Maze.dimensions maze
       let revealed = [for x in 0..xdim-1 do for y in 0..ydim-1 do yield (x,y)]
       { model with revealed = Set.ofList revealed }, []
    | None -> model, []
  | SwitchAlgorithm(alg) ->
    { model with
        mazeGenerator =
          (match alg with
            | Binary -> binaryMaze
            | HunterKiller -> huntKill
            | ABW -> aldousBroderWilsons
          ) 22 10
        algorithm = alg
      },
      [fun d -> d Refresh]

module Key =
  let left = KeyDetect 37
  let up = KeyDetect 38
  let right = KeyDetect 39
  let down = KeyDetect 40

type DisplayObject with
  member this.finish = ()

let ValueStr str = DefaultValue <| U2.Case1 str

let debugmenow e =
  System.Diagnostics.Debugger.Break()
  ()

let view (model: ViewModel) dispatch =
  Key.left.Pressed <- delay dispatch (Move Left)
  Key.right.Pressed <- delay dispatch (Move Right)
  Key.up.Pressed <- delay dispatch (Move Up)
  Key.down.Pressed <- delay dispatch (Move Down)

  R.div [ClassName "shell"] [
    R.div [] [
      R.div [] [
        R.h2 [] [R.str "Use the arrow keys to move around the dungeon until you find the blue X. The faster you find it, the more points you get. Watch out for monsters!"]
        ]
      R.h4 [] [R.str (sprintf "Points: %d      Steps: %d" model.points model.steps)]
      R.form [] [
        R.label [HtmlFor "algorithmSelector"] [R.str "Maze generation algorithm"]
        R.select [Id "algorithmSelector"; ValueStr (Algorithm.Render model.algorithm); OnSelect(fun e -> debugmenow e); OnChange(fun e -> dispatch <| SwitchAlgorithm((((e.target |> unbox<obj>)?value) |> unbox string |> Algorithm.Parse)))] (
          Algorithm.All
          |> List.map (fun alg -> R.option [OnSelect (fun _ -> dispatch (SwitchAlgorithm alg))][R.str (Algorithm.Render alg)])
          )
        ]
      R.div [] [
        R.button [OnClick (fun _ -> dispatch Refresh)] [R.str "New Maze"]
        R.button [OnClick (fun _ -> dispatch Reveal)] [R.str "Reveal Maze"]
        ]
      R.div [] [
        R.text [] [
          match model.messages |> List.filter (fun (x,y,msg) -> (x,y) = model.currentPosition) with
          | [] -> "Nothing interesting here"
          | msgs -> String.Join("; also ", msgs |> List.map(fun(_,_,msg) -> msg))
          |> R.str
          ]
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
        let wallColor = float 0x696969
        let fogOfWarColor = float 0xA9A9A9
        let wallWidth = 4.
        let halfWall = wallWidth/2.
        let emptyColor = float 0xFFFFFF
        for x in 0 .. xlen - 1 do
          for y in 0.. ylen - 1 do
            let left = (float x) * cellWidth
            let right = (float x + 1.) * cellWidth
            let top = (float y) * cellHeight
            let bottom = (float y + 1.) * cellHeight
            if not (model.revealed |> Set.contains (x,y)) then
              // draw fog of war
              g.lineStyle(0.).beginFill(fogOfWarColor).drawRect(left+halfWall, top+halfWall, cellWidth-wallWidth, cellHeight-wallWidth).endFill().finish

            // draw walls if you can see them, giving a hint of where the openings are
            let color =
              if not (model.revealed.Contains (x,y) || model.revealed.Contains(x+1,y)) then
                fogOfWarColor
              elif not <| Maze.rightOpen maze (x,y) then
                wallColor
              else emptyColor
            g.lineStyle(wallWidth, color).moveTo(right, top).lineTo(right, bottom).finish

            let color =
              if not (model.revealed.Contains (x,y) || model.revealed.Contains(x,y+1)) then
                fogOfWarColor
              elif not <| Maze.downOpen maze (x,y) then
                wallColor
              else emptyColor
            g.lineStyle(wallWidth, color).moveTo(left, bottom).lineTo(right, bottom).finish
        // draw exterior walls
        g.lineStyle(wallWidth,wallColor).moveTo(0.,halfWall).lineTo(w,halfWall).moveTo(halfWall,0.).lineTo(halfWall,h)
          .moveTo(0.,h-halfWall).lineTo(w,h-halfWall).moveTo(w-halfWall,0.).lineTo(w-halfWall,h).finish
        // draw goal as blue X
        let left, top =
          match model.goal with
          | (x,y) -> toX x, toY y
        g.lineStyle(5., float 0x0011FF)
          .moveTo(left, top)
          .lineTo(left + cellWidth, top + cellHeight)
          .moveTo(left + cellWidth, top)
          .lineTo(left, top + cellHeight)
          .finish

        // Also, draw the current position in red
        let left, top =
          match model.currentPosition with
          | (x,y) -> toX x, toY y
        g.lineStyle(0.).beginFill(float 0x891121)
          .drawRect(left + 10., top + 10., cellWidth - 20., cellHeight - 20.) // leave 10-pixel margin because it looks good
          .endFill().finish
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