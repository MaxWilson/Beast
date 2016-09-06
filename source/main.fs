module Beast

open Fable.Core 
open Fable.Import
 
Node.require.Invoke("core-js") |> ignore

let element = Browser.document.getElementById "sample"
element.innerText <- "Hello, world!"