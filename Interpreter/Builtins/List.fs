module Interpreter.Builtins.List

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Value

let listMembers: Context = Dictionary()
let listType = Type("List", listMembers)
let nodeCons = Constructor("Node", 2, listType)
let endCons = Constructor("End", 0, listType)

let isList stack v =
    let evaluated = dereference stack v
    objIsInst listType evaluated

listMembers.Add("inst", Val(Function(isList)))
