module Interpreter.Builtins.BuiltinFunctions

open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.RefUtil
open Interpreter.Value

let andOp =
    Function(fun st v ->
        if not (checkCondition st v) then
            Function(fun _ _ -> v)
        else
            Function(fun _ v' -> v'))

let orOp =
    Function(fun st v ->
        if (checkCondition st v) then
            Function(fun _ _ -> v)
        else
            Function(fun _ v' -> v'))

let applicationOp = Function(fun _ v -> Function(fun _ v' -> Application(v, v')))

let reverseApplicationOp =
    Function (fun _ v -> Function (fun _ v' -> Application(v', v)))

let listConsOp = Function (fun _ v -> Function (fun _ -> node' v))

let repr =
    Function(
        fun st v ->
            let (Type(_, ns)) = getType st v
            let reprMember = Ref(ns, topLevelGetter "repr", topLevelSetter "repr")
            Application(reprMember, v)
    )
    
let print =
    Function(
        fun st v ->
            Action(
                fun () ->
                    let (String s) = dereference st (Application(reprRef.Value, v))
                    printf $"%s{s}"
                    none'()
            )
    )
