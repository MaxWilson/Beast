﻿module Components

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

[<Pojo>]
type CBState = { data: string }

[<Pojo>]
type CBProps = { nothing: int }

type HelloBox(props: CBProps) as this =
    inherit React.Component<CBProps, CBState>(props)
    do this.setInitState({ data = "Hello world!" })

    member x.handleSubmit (e: FormEvent) =
        let msg  = x.state.data
        x.setState { data = msg + "!" }
        e.preventDefault()

    member x.componentDidMount () = ()

    member x.render () =
        R.div [ClassName "commentBox"] [
            // Use ReactHelper.com to build a React Component from a type
            R.text [] [unbox x.state.data]
            R.input [
                Type "submit"
                Value (U2.Case1 "OK")
                OnClick (fun _ -> x.setState { data = x.state.data + "!" })
            ] []
        ]

[<Pojo>]
type PickListProps<'t> =
    { options: 't list; renderToString: 't -> string }
[<Pojo>]
type PickListState<'t> =
    { selected: 't list }

type PickList<'t>(props: PickListProps<'t>) as this =
    inherit Component<PickListProps<'t>, PickListState<'t>>(props)
    do this.setInitState({ selected = []})
    member x.render() =
        R.div [] [
            R.input [DefaultValue (U2.Case1 "")] []
            R.div [] (
                props.options |> List.map (fun o -> R.div [] [R.button [] [R.str (props.renderToString o)]])
                )
            R.div [] [R.str (JsInterop.toJson props.options)]
            ]

