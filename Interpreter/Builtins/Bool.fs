module Interpreter.Builtins.Bool

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Value

let boolMembers: Context = Dictionary() |> applyDefaultRepr |> applyDefaultInequality
let boolType = Type("Bool", boolMembers)
let trueCons = Constructor("True", 0, boolType)
let falseCons = Constructor("False", 0, boolType)

let isBool stack (v: Value) =
    let toApply v =
        let evaluated = dereference stack v
        objIsInst boolType evaluated
    ApplyAfter(v, stack, defaultForceStop, [toApply], [], [])

boolMembers.Add("inst", Val(Function(isBool)))
let boolTruth st v =
    let toApply v =
        let ev = dereference st v
        match ev with
        | Object(Constructor(_, _, type'), _) when typeEq type' boolType -> ev
        | _ -> raise (unexpectedType "Bool.truth")
    ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
boolMembers.Add("truth", Val(Function(boolTruth)))

let boolEq =
    Function(
        fun st v ->
            let toApply v =
                Function(
                    fun st' v' ->
                        let toApply v' =
                            let ev = dereference st v
                            let ev' = dereference st' v'
                            match ev with
                            | Object(Constructor(name, _, type'), _) when typeEq type' boolType ->
                                match ev' with
                                | Object(Constructor(name', _, type''), _) when typeEq type'' boolType ->
                                    if name = name' then true'() else false'()
                                | _ -> false'()
                            | _ -> raise (unexpectedType "Bool.(=)")
                        ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
                )
            ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
boolMembers.Add("(=)", Val boolEq)

let boolNeg =
    Function(
        fun st v ->
            let toApply v =
                if satisfiesConstructor st trueCons v then false'()
                elif satisfiesConstructor st falseCons v then true'()
                else raise (unexpectedType "Bool.(!)")
            ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
boolMembers.Add("(!)", Val boolNeg)