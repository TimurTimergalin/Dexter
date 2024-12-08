module Interpreter.Builtins.NoneType

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Value

let noneTypeMembers: Context = Dictionary()
let noneType = Type("NoneType", noneTypeMembers)
let noneCons = Constructor("None", 0, noneType)

let isNone stack v =
    let evaluated = dereference stack v
    objIsInst noneType evaluated

noneTypeMembers.Add("inst", Val(Function(isNone)))
let noneTruth _ _ = false' ()
noneTypeMembers.Add("truth", Val(Function(noneTruth)))
