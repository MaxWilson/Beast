module Components

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Models

// ReactHelper defines a DSL to make it easier to build
// React components from F#
module R = Fable.Helpers.React
open R.Props
open Fable.Import.React
open Fable.Import.PIXI
open Fable.Import.Browser

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
