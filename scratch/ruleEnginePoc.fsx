module ruleEnginePoc
#load "..\source\Heap.fs"
open Heap 
type State = int
type Operation = Plus | Minus | Times | Divide
type Delta = Operation * int
type Deltas = Delta list
let resolve state = function
| Plus, x -> state + x
| Minus, x -> state - x
| Times, x -> state * x
| Divide, x -> state / x
let resolveDeltas (state: State) (deltas: Deltas) : State = List.fold resolve state deltas

type Priority = int
type ModelState = State * Deltas
type RuleConsequent = Priority * ModelState -> (Delta list)
type Rule = ModelState -> RuleConsequent list
type ComputationState = ModelState * Heap<RuleConsequent>
resolveDeltas 4 [Times, 5; Plus, 2; Divide, 2]

Heap.empty