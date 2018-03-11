open System

let flip f x y = f y x

let rand = Random()

type NodeUnfold<'world, 'node, 'accum> = ('world -> 'accum -> ('node * 'accum) option)
type NodeResult<'world, 'node, 'accum> = ('node * ('accum * NodeUnfold<'world, 'node, 'accum>) option) option
type Tree<'node> = Node of 'node * successors: Tree<'node> list | Leaf of 'node
// build a tree, threading state through it in DFS fashion
let unfold (updateState: 'world -> 'node -> 'world) (deduceNodes: 'world -> 'node list -> NodeResult<'world, 'node, 'accum>) (predecessors: 'node list) (world: 'world) : Tree<'node> * 'world =
  let update world n =
    updateState world n
  let rec loopdfs predecessors (world: 'world): Tree<'node> list * 'world =
    match predecessors |> deduceNodes world with
    | None -> failwith "Not implemented"
    | Some(node, None) ->
      [Leaf node], update world node
    | Some(firstChild, Some(acc, makeMore)) ->
      let makeTreenode (node, acc) world =
        let world = update world node
        let grandchildren, world = loopdfs [node] world 
        let treenode = Node(node, grandchildren)
        treenode, (acc, world)
      let (firstChild, (acc, world)) = makeTreenode (firstChild, acc) world
      let otherGrandchildren = 
        flip List.unfold (acc, world) <| fun (acc, world) ->
          makeMore world acc |> Option.map (flip makeTreenode world)
      firstChild :: otherGrandchildren, world
  match loopdfs predecessors world with
  | [node], world -> node, world
  | _ -> failwith "Current implementation can only handle tree, not forest, at the root level"

// example usage of Tree.unfold. The rule here is, "Starting from n=1, produce two children (2n) and (2n+1), unless "
let treeCount maxN =

  let checkedDoubling count lastVal =
    if count >= maxN then None
    else Some((lastVal * 2 + 1).ToString(), (lastVal * 2 + 1))

  let makeChildren w preds =
    match preds with
    | [parent] ->
      let parent =
        match System.Int32.TryParse(parent) with
        | false, _ -> failwith "Unable to parse one of the numbers"
        | true, n -> n
      Some((parent*2).ToString(), Some(parent, checkedDoubling))
    | _ -> failwith "Should never happen when using Tree (because it doesn't support multiple predecessors)"

  // return the resulting tree
  0 |> unfold (fun count _ -> count + 1) makeChildren ["1"] |> fst

// check expectations
treeCount 6 = Node("1", [Node("2", [Leaf "4"; Leaf "5"]); Node("3", [Leaf "6"])])
treeCount 4 = Node("1", [Node("2", [Leaf "4"; Leaf "5"])])