open System

let flip f x y = f y x

let rand = Random()

type NodeUnfold<'world, 'node, 'accum> = ('world -> 'accum -> ('node * 'accum) option)
type NodeResult<'world, 'node, 'accum> = 'accum * NodeUnfold<'world, 'node, 'accum>
type Tree<'node> = Node of 'node * successors: Tree<'node> list | Leaf of 'node
// build a tree, threading state through it in DFS fashion
let unfold (updateState: 'world -> 'node -> 'world) (deduceNodes: 'world -> 'node list -> NodeResult<'world, 'node, 'accum>) (predecessors: 'node list) (world: 'world) : Tree<'node> * 'world =
  let update world n =
    updateState world n
  let rec loopdfs predecessors (world: 'world): Tree<'node> list * 'world =
    let acc, makeNext = predecessors |> deduceNodes world
    let makeTreenode (node, acc) world =
      let world = update world node
      let grandchildren, world = loopdfs [node] world 
      let treenode = match grandchildren with [] -> Leaf(node) | _ -> Node(node, grandchildren)
      treenode, (acc, world)
    let rec unfoldWithState f st =
      match f st with
      | Some(v, st) -> 
        let others, st = (unfoldWithState f st)
        v::others, st
      | None -> [], st
    let children, (_, world) = 
      flip unfoldWithState (acc, world) <| fun (acc, world) ->
        makeNext world acc |> Option.map (flip makeTreenode world)
    children, world
  match predecessors, loopdfs predecessors world with
  | [node], (children, world) -> Node(node, children), world
  | nodes, (_, _) -> failwithf "Current implementation can only handle tree, not forest, at the root level. Got %A" nodes


// example usage of Tree.unfold. The rule here is, "Starting from n=1, produce two children (2n) and (2n+1), unless "
let treeCount maxN maxParent =

  let yielder count accum = 
    match accum with | h::t when count < maxN -> Some(h, t) | _ ->  None

  let deduceChildren _ preds =
    match preds with
    | [parent] ->
      let parent =
        match System.Int32.TryParse(parent) with
        | false, _ -> failwith "Unable to parse one of the numbers"
        | true, n -> n
      if parent > maxParent then [], yielder
      else
        ([parent*2;parent*2+1] |> List.map (sprintf "%d")), yielder
    | _ -> failwith "Should never happen when using Tree (because it doesn't support multiple predecessors)"

  // return the resulting tree
  1 |> unfold (fun count _ -> count + 1) deduceChildren ["1"] |> fst

// check expectations
treeCount 6 3 = Node("1", [Node("2", [Leaf "4"; Leaf "5"]); Node("3", [Leaf "6"])])
treeCount 4 3 = Node("1", [Node("2", [Leaf "4"; Leaf "5"])])
treeCount 3 1 = Node("1", [Leaf "2"; Leaf "3"])