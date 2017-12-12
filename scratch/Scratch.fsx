open System.Web.UI.WebControls

module Queue =
    type queue<'a> =
        private | Queue of 'a list * 'a list
    let empty = Queue([], [])
    let push e q = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)
    let pop q = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue([], bs.Tail)
    let isEmpty =
        function
        | Queue([], []) -> true
        | _ -> false

type StaticState = int
type IntendedEffect =
    | Plus of int option
    | Minus of int option
    | Times of int option
    | Divide of int option
type AppliedEffect =
    | Plus of int
    | Minus of int
    | Times of int
    | Divide of int
type PresentState = AppliedEffect list * StaticState
type ComputationState = Queue.queue<IntendedEffect> * PresentState

let resolve (state: StaticState) (delta: AppliedEffect) =
    match delta with
    | Plus(x) -> state + x
    | Minus(x) -> state - x
    | Times(x) -> state * x
    | Divide(x) -> state / x

// test
(List.fold resolve 3 [Plus(8); Times(2); Minus(3); Divide(4)]) = 4

type Result = Substitute of IntendedEffect * ComputationState | Do of AppliedEffect * ComputationState

type Ruleset<'t>(resolve,?trySubstitute,?chainedEffect) =
    let subs = defaultArg trySubstitute []
    let chain = defaultArg chainedEffect []
    member this.Apply(action: IntendedEffect, computationState : ComputationState) =
        // look for a substitution
        let substitution = (subs |> List.tryPick (fun rule ->
            match rule (action, computationState) with
            | Some(action : IntendedEffect, computationState: ComputationState) ->
                // trampoline: found a substitution: abort this chain, look for a new one                
                None
            | None ->
                Some(action, computationState)))
        match substitution with
        | Some(action, computationState) -> Substitute(action, computationState)
        | None -> Do(resolve action)
    member this.Chain(action: AppliedEffect, computationState: ComputationState) =
        // look for any additions
        let doRule computationState rule =
            // any rule that returns Some(computationState) affects the future
            defaultArg (rule (action, computationState)) computationState
        chain |> List.fold doRule computationState