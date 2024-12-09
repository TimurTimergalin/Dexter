module Interpreter.Builtins.Bool

open System.Collections.Generic
open Interpreter.Evaluate
open Interpreter.Value

let boolMembers: Context = Dictionary() |> applyDefaultRepr
let boolType = Type("Bool", boolMembers)
let trueCons = Constructor("True", 0, boolType)
let falseCons = Constructor("False", 0, boolType)

let isBool stack (v: Value) =
    let evaluated = dereference stack v
    objIsInst boolType evaluated

boolMembers.Add("inst", Val(Function(isBool)))
let boolTruth stack v = v
boolMembers.Add("truth", Val(Function(boolTruth)))
