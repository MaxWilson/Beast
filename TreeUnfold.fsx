open System
let rand = Random()

type nodeUnfold<'world, 'node, 'accum> = ('world -> 'accum -> ('node * 'accum) option)
type nodeResult<'world, 'node, 'accum> = ('node * ('accum * nodeUnfold<'world, 'node, 'accum>) option) option
type Tree<'node> = Node of 'node * successors: Tree<'node> list | Leaf of 'node
// build a tree, threading state through it in DFS fashion
let unfold (updateState: 'world -> 'node -> 'world) (deduceNodes: 'world -> 'node list -> nodeResult<'world, 'node, 'accum>) (predecessors: 'node list) (world: 'world) : Tree<'node> =
  let update world n =
    updateState world n
  let rec loop world =
    match predecessors |> deduceNodes world with
    | None -> failwith "Not implemented"
    | Some(node, None) ->
      Leaf node, update world node
    | Some(node, Some(acc, makeMore)) ->
      let children = (world, acc) |> List.unfold (fun (world, accum) -> let (world, accum) = makeMore )
      Leaf node, update world node
  loop world |> fst

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
  0 |> unfold (fun count _ -> count + 1) makeChildren ["1"]

// check expectations
treeCount 6 = Node("1", [Node("2", [Leaf "4"; Leaf "5"]); Node("3", [Leaf "6"])])
treeCount 4 = Node("1", [Node("2", [Leaf "4"; Leaf "5"])])
