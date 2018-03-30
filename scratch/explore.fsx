<<<<<<< Updated upstream
#r @"..\bin\Fable.Core.dll"
#r @"..\bin\Fable.React.dll"
#load @"..\source\Models.fs"
#load @"..\source\Components.fs"
open Models
||||||| merged common ancestors
type OpType = Plus | Minus | Times | Divide
type Delta = OpType * int
let resolveDelta state delta =
  match delta with
  | Plus, x -> state + x
  | Minus, x -> state - x
  | Times, x -> state * x
  | Divide, x -> state / x
let resolveDeltas state deltas =
  List.fold resolveDelta state deltas
type GameState = int * Delta list
module PriorityQueue =
  type D<'t> = (int * 't) list
  let add (pri: int, data: 't) (q:D<'t>) =
    (pri, data)::q |> List.sortBy fst
  let (|Pop|_|) (q:D<'t>) =
    match q with
    | [] -> None
    | (pri, data)::rest -> Some(data, rest)

type ActionHeap = PriorityQueue.D<int * (GameState -> GameState list)>
type ComputationState = GameState * ActionHeap
resolveDeltas 10 [Plus, 5; Times, 2; Divide, 6]

let q = [] |> PriorityQueue.add(2, (Times, 2)) |> PriorityQueue.add(3, (Divide, 6)) |> PriorityQueue.add(1, (Plus, 5))
match q with PriorityQueue.Pop(d, rest) -> printfn "%A" d | _ -> ()
=======
type OpType = Plus | Minus | Times | Divide
type CompoundOp = Op of OpType * int | CompoundOp of CompoundOp list
  with
  static member Create(ops: (OpType * int) list) =
    ops |> List.map Op |> CompoundOp
let op t x = Op(t, x)
type Delta = ComplexOp
let rec resolveDelta state delta =
  match delta with
  | Op(op, x) ->
    match op, x with
    | Plus, x -> state + x
    | Minus, x -> state - x
    | Times, x -> state * x
    | Divide, x -> state / x
  | CompoundOp(ops) ->
    List.fold resolveDelta state ops
type GameState = int * Delta list
module PriorityQueue =
  type D<'t> = (int * 't) list
  let add (pri: int, data: 't) (q:D<'t>) =
    (pri, data)::q |> List.sortBy fst
  let (|Pop|_|) (q:D<'t>) =
    match q with
    | [] -> None
    | (pri, data)::rest -> Some(data, rest)

type ActionHeap = PriorityQueue.D<int * (GameState -> GameState list)>
type ComputationState = GameState * ActionHeap
resolveDelta 10 ([Plus, 5; Times, 2; Divide, 6] |> CompoundOp.Create)

let q = [] |> PriorityQueue.add(2, (Times, 2)) |> PriorityQueue.add(3, (Divide, 6)) |> PriorityQueue.add(1, (Plus, 5))
match q with PriorityQueue.Pop(d, rest) -> printfn "%A" d | _ -> ()

let fixpoint (resolvePrimitive: 'state -> 'command -> 'state) (ruleChaining : 'state-> 'command option) (state: 'state) (changes: 'command list) =
  let rec applyRules state change =
    let state = resolvePrimitive state change // shadow state with new state
    // printf "%A " state
    match state |> ruleChaining with
    | Some(newDelta) ->
      applyRules state newDelta
    | None -> state
  let initialChanges =
    match ruleChaining state with // since we chain on state as well as deltas, give it a chance to chain first thing
    | Some(c) -> c :: changes
    | None -> changes
  List.fold (fun state delta -> applyRules state delta) state initialChanges

type Set<'a> = Set of 'a
let chainArithmetic : int -> _ -> int =
  fixpoint (fun x (Set y) -> y) (function | x when x <= 1 -> None | x when x % 2 = 0 -> Some (Set <| x/2) | x -> Some(Set <| x*3 + 1))
chainArithmetic 4 []

let collatzConjecture: int -> CompoundOp list -> int =
  let cc x = Some (CompoundOp.Create x)
  fixpoint resolveDelta (function | x when x <= 1 -> None | (x: int) when x % 2 = 0 -> Some(Op(Divide, 2)) | x -> Some(CompoundOp.Create [Times, 3; Plus, 1]))
collatzConjecture 5 []

for x in 1..100 do
  printf "\n%d " x
  collatzConjecture x [] |> ignore

// 1.) react to an action
// 2.) react to a state
// Add to beginning of delta queue, or add to end of delta queue.

module SharedState =
  type IndexNode<'t> = { data: 't; self: int; replacement: int option; predecesssors: int list; successors: int list; }
  type SharedState<'t> = { root: int; firstUnusedIndex: int; data: Map<int, IndexNode<'t>>; }
  /// Get data for an IndexNode from its tree
  let get ix (state: SharedState<_>) =
    state.data.[ix].data
  let succ ix (state: SharedState<_>) =
    state.data.[ix].successors
  let pred ix (state: SharedState<_>) =
    state.data.[ix].predecesssors
  let set (node: IndexNode<_>) (state: SharedState<_>) =
    { state with data = state.data |> Map.add node.self node }
  let init(data) =
    let i = 0;
    let node = { data = data; self = i; replacement = None; predecesssors = []; successors = [] }
    { root = i; firstUnusedIndex = i + 1; data = Map.empty |> Map.add i node }, i
  let addSuccessor (ix:int) (successorData: 't) (state: SharedState<'t>) =
    let ix' = state.firstUnusedIndex;
    let node = { data = successorData; self = ix'; replacement = None; predecesssors = [ix]; successors = [] }
    let current = state.data.[ix]
    let current = { current with successors = ix' :: current.successors }
    { state with data = state.data |> Map.add ix' node |> Map.add ix current; firstUnusedIndex = ix' + 1 }, ix'
  let addSuccessorM (ixs:int list) (successorData: 't) (state: SharedState<'t>) =
    let ix' = state.firstUnusedIndex;
    let node = { data = successorData; self = ix'; replacement = None; predecesssors = ixs; successors = [] }
    let augmented = ixs |> List.fold (fun (m: Map<_, _>) ix -> let c = m.[ix] in m |> Map.add ix { c with successors = ix' :: c.successors }) state.data
    { state with data = augmented |> Map.add ix' node; firstUnusedIndex = ix' + 1 }, ix'
  let dfsFold (f: 'u -> 't -> 'u) (accum: 'u) (state: SharedState<'t>) =
    let rec dfs accum ix =
      let node = state.data.[ix]
      let accum = f accum node.data
      node.successors |> List.fold dfs accum
    dfs accum state.root
open SharedState

let st, root = SharedState.init("m")
let st, a = st |> addSuccessor st.root "a"
let st, x = st |> addSuccessorM [root; a] "x"
st |> dfsFold (fun () -> printfn "%s") ()
st
succ a st
pred a st
>>>>>>> Stashed changes
