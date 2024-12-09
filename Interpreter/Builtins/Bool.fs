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
    let evaluated = dereference stack v
    objIsInst boolType evaluated

boolMembers.Add("inst", Val(Function(isBool)))
let boolTruth st v =
    let ev = dereference st v
    match ev with
    | Object(Constructor(_, _, type'), _) when typeEq type' boolType -> ev
    | _ -> raise (unexpectedType "Bool.truth")
boolMembers.Add("truth", Val(Function(boolTruth)))

let boolEq =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Object(Constructor(name, _, type'), _) when typeEq type' boolType ->
                        match ev' with
                        | Object(Constructor(name', _, type''), _) when typeEq type'' boolType ->
                            if name = name' then true'() else false'()
                        | _ -> false'()
                    | _ -> raise (unexpectedType "Bool.(=)")
            )
    )
boolMembers.Add("(=)", Val boolEq)

let boolNeg =
    Function(
        fun st v ->
            if satisfiesConstructor st trueRef.Value v then false'()
            elif satisfiesConstructor st falseRef.Value v then true'()
            else raise (unexpectedType "Bool.(!)")
    )