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
    nums: Heap<int>
  }
  with
  static member create msg = { message = msg; count = 1; nums = Heap.empty(false) }
type Msg = | NewString of string | Increment | RandomNumber | PopNumber
let init _ = Model.create "Hello", []
let update msg model =
  match msg with
  | NewString(txt) ->
    { model with message = txt }, [fun dispatch -> dispatch Increment]
  | Increment -> { model with count = model.count + 1 }, []
  | RandomNumber ->
    let r = System.Random()
    let n = r.Next(100) + 1
    { model with nums = Heap.insert n model.nums }, []
  | PopNumber ->
    { model with nums = match model.nums with | Heap.Cons(_, rest) -> rest | rest -> rest }, []

let view (model:Model) dispatch =
  R.div [ClassName "shell"] [
    PixiBox.Create (fun (w:Width, h:Height) ->
      let stage = Container()
      let rec getHeap (hp : Heap<_>) =
        match hp with
        | Heap.Nil -> ""
        | Heap.Cons(data, rest) -> 
            match rest with
            | Heap.Nil -> data.ToString()
            | _ ->
                (data.ToString()) + ", " + (getHeap rest)
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
      stage.addChild(Text((getHeap model.nums),
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
      R.button [OnClick (fun _ -> dispatch RandomNumber)] [R.str "Random"]
      R.button [OnClick (fun _ -> dispatch PopNumber)] [R.str "Pop"]
      ]
    ]

Program.mkProgram init update view
 //|> Program.toNavigable
 |> Program.withReact "content"
 |> Program.run