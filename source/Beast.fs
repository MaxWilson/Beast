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

open Util
open Models
open System
open Stat

type BeastUI() =
    inherit Component<obj, obj>()
    member this.render() =
        R.div [] (this.children |> List.ofArray)

ReactDom.render(
    R.div [] [
        R.h1 [] [R.str "BEAST 5E"]
        R.h3 [] [R.str "Brain-dead-simple Simulator Tool for 5E combats"]
        R.com<BeastUI,_,_> (obj()) [R.str "placeholder"]
        ],
    Browser.document.getElementById "content")
|> ignore