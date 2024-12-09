module Interpreter.Builtins.NoneType

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Value

let noneTypeMembers: Context = Dictionary() |> applyDefaultNegation |> applyDefaultInequality |> applyDefaultTruth |> applyDefaultRepr
let noneType = Type("NoneType", noneTypeMembers)
let noneCons = Constructor("None", 0, noneType)

let isNone stack v =
    let evaluated = dereference stack v
    objIsInst noneType evaluated
noneTypeMembers.Add("inst", Val(Function(isNone)))

let noneTypeEq =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    if not (satisfiesConstructor st noneCons v) then
                        raise (unexpectedType "NoneType.(=)")
                    if satisfiesConstructor st' noneCons v' then true'() else false'()
            )
    )
noneTypeMembers.Add("(=)", Val noneTypeEq)