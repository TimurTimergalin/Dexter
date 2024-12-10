module Interpreter.Builtins.List

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Value

let listMembers: Context = Dictionary() |> applyDefaultTruth |> applyDefaultEquality |> applyDefaultInequality |> applyDefaultNegation
let listType = Type("List", listMembers)
let nodeCons = Constructor("Node", 2, listType)
let endCons = Constructor("End", 0, listType)

let isList stack v =
    let toApply v =
        let evaluated = dereference stack v
        objIsInst listType evaluated
    ApplyAfter(v, stack, defaultForceStop, [toApply], [], [])
listMembers.Add("inst", Val(Function(isList)))

    
let rec toInternalList stack value acc =
    let ev = dereference stack value
    if not (typeEq (getType stack ev) listType) then
        raise (unexpectedType "List.repr")
    if satisfiesConstructor stack endCons ev then
        acc
    else
        let (Object(_, args)) = ev
        let (String s) = dereference stack (Application(reprRef.Value, args[0]))
        toInternalList stack args[1] (s::acc)

let listRepr stack v =
    let toApply v =
        let internalList = toInternalList stack v []
        let repr = "[" + String.concat ", " (List.rev internalList) + "]"
        String repr
    ApplyAfter(v, stack, defaultForceStop, [toApply], [], [])
listMembers.Add("repr", Val(Function listRepr))
