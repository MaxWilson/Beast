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

type Model = {
    message: string
    count: int
  }
  with
  static member create msg = { message = msg; count = 1 }
type Msg = | NewString of string | Increment
let init _ = Model.create "Hello", []
let update msg model =
  match msg with
  | NewString(txt) ->
    { model with message = txt }, [fun dispatch -> dispatch Increment]
  | Increment -> { model with count = model.count + 1 }, []

type Height = float
type Width = float

[<Pojo>]
type PixiBoxProps<'t when 't :> DisplayObject> = { render: (Width * Height) -> 't }

type PixiBox<'t when 't :> DisplayObject>(props) =
  inherit React.Component<PixiBoxProps<'t>, obj>(props)
  let mutable canvasContainer: HTMLElement = null
  let mutable renderer : SystemRenderer option = None
  let renderGraphics() =
    if canvasContainer <> null then
      if renderer.IsNone then
        renderer <- Globals.autoDetectRenderer(canvasContainer.clientWidth, canvasContainer.clientHeight, [RendererOptions.BackgroundColor (float 0x1099bb); Resolution 1.; Transparent true]) |> unbox<SystemRenderer> |> Some
        canvasContainer.appendChild(renderer.Value.view) |> ignore
      renderer.Value.render(props.render(canvasContainer.clientWidth * 0.9, canvasContainer.clientHeight * 0.9))
  member this.render() =
    R.div [ClassName "pixiBox"; Ref (fun x -> canvasContainer <- (x :?> HTMLElement); renderGraphics())] []
  member this.componentDidMount() =
    renderGraphics()
  static member Create<'t when 't :> DisplayObject>(render: (Width * Height) -> 't) = R.com<PixiBox<'t>, _, _>({ render = render }) []

let view (model:Model) dispatch =
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