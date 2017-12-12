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