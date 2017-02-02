/// syntactic sugar and F# utility functions
module Util
open System
open Fable.Core

// System.Random does not exist in JavaScript, but we want our code to work in both .NET and Random, so we expose methods random and randomRational which work in both environments
[<Emit("null")>]
let private makeRandom() =
    new Random()
let private r = makeRandom()

/// Returns a number between 1 and n
[<Emit("Math.floor(Math.random() * $0) + 1")>]
let random(n:int) =
    r.Next(n) + 1

// Returns a number between 0 and n
[<Emit("Math.random() * $0")>]
let randomRational(n:float) =
    r.NextDouble() * n

let flip0 sb f = f sb
let flip f x y = f y x
let apply args f = f args

let badMatch sourceFile lineNumber argMatch = failwithf "%s line %s has bug: failed to match '%A'" sourceFile lineNumber argMatch
