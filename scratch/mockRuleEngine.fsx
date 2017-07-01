
#load @"..\source\Packrat.fs"
open Packrat
let pack = Packrat.pack
let alpha = Set<char>['A'..'Z'] + Set<char>['a'..'z']
let numeric = Set<char>['0'..'9']
let alphanumeric = Set.union alpha numeric

let (|Next|Empty|) = function
    | ({ input = input } : ParseContext as ctx), pos when pos < input.Length -> Next(input.[pos], (ctx, pos+1))
    | v -> Empty

let (|Char|_|) alphabet = function
    | Empty -> None
    | s, i when Set.contains s.input.[i] alphabet -> Some(s, i+1)
    | _ -> None

let rec (|MaybeChars|) alphabet = function
    | Char alphabet (MaybeChars alphabet it)
    | it -> it

let rec (|Chars|_|) alphabet = function
    | Char alphabet (MaybeChars alphabet (ctx, endpos)) as input ->
      let _, startpos = input
      Some (ctx.input.Substring(startpos, endpos - startpos), (ctx, endpos))
    | it -> None

let (|NextChar|_|) alphabet = function
    | Empty -> None
    | s, i when Set.contains s.input.[i] alphabet -> Some(s.input.[i], (s, i+1))
    | _ -> None

let sub (s: string, i0) (_, i1) =
    s.Substring(i0, i1-i0)

// need a better way of specifying optional characters like ' ' instead of combinatoric patterns
let (|NextWord|_|) = pack (function
    | Next(' ', Chars alphanumeric (word, Next(' ', rest))) -> Some(word.Trim().ToLowerInvariant(), rest)
    | Next(' ', Chars alphanumeric (word, rest)) -> Some(word.Trim().ToLowerInvariant(), rest)
    | Chars alphanumeric (word, Next(' ', rest)) -> Some(word.Trim().ToLowerInvariant(), rest)
    | Chars alphanumeric (word, rest) -> Some(word.Trim().ToLowerInvariant(), rest)
    | _ -> None)

let rec (|Number|_|) = pack (function
    | Chars numeric (chars, rest) -> (System.Int32.Parse(chars), rest) |> Some
    | _ -> None)

let mutable world = Map.empty<string, obj>
type Direction = North | South | East | West
type Cmd =
  | Init of int * int
  | Add of string * int * (Direction option)

let (|Root|_|) = pack (function
  | NextWord("init", Number(x, Next('x', Number(y, endi)))) -> Some (Init(x, y), endi)
  | NextWord("add", Number(n, NextWord(name, NextWord(dir, endi)))) ->
    let d = match dir with | "north" -> Some North | "south" -> Some South | "east" -> Some East | "west" -> Some West | _ -> None
    printfn "%A" (Add(name, n, d), endi)
    Some(Add(name, n, d), endi)
  | NextWord("add", Number(n, NextWord(name, endi))) -> Some (Add(name, n, None), endi)
  | _ -> None
  )

let parse txt = match ParseContext.Init txt with
  | Root(result, Empty) -> result
  | (ctx, _) -> failwithf "Could not parse: '%s'\n%A" txt ctx

let update (world: Map<string, obj>) cmd =
  match cmd with
  | Init(x, y) -> world |> Map.add "size" (box (x,y))
  | Add(monster, n, d) ->
    let name = (match d with Some (d) -> monster + sprintf "%A" d | _ -> monster)
    match world.TryFind("beasties") with
    | Some(:? list<int*string> as lst) ->
      world |> Map.add "beasties" (box ((n, name)::lst))
    | _ ->
      world |> Map.add "beasties" (box [n, name])

match ParseContext.Init "add 40 orogs south" with
| NextWord("add", Number(n, NextWord(name, NextWord(dir, endi)))) ->
  let d = match dir with | "north" -> Some North | "south" -> Some South | "east" -> Some East | "west" -> Some West | _ -> None
  printfn "%A" (Add(name, n, d), endi)
| Number(n, NextWord(name, endi)) -> printf "%A" (n, endi)
| Number(n, rest) -> printf "%A" (n, rest)

world <- (update world (parse "Init 300x300"))
world <- (update world (parse "add 40 orogs"))
world <- (update world (parse "Add 40 orogs south"))
printfn "%A" world
