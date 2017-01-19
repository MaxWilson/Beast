module Beast

open System
open Fable.Core
open Fable.Import
module R = Fable.Helpers.React

// Check components.fs to see how to build React components from F#
open Components

// Polyfill for ES6 features in old browsers
Node.require.Invoke("core-js") |> ignore

ReactDom.render(
    R.div [] [
        R.h1 [] [R.str "BEAST 5E"]
        R.h3 [] [R.str "Brain-dead-simple Simulator Tool for 5E combats"]
        R.text [] [PickList ["Gargoyle";"Banderhobb";"Bandersnatch";"Kzin"]]
        ],
    Browser.document.getElementById "content")
|> ignore