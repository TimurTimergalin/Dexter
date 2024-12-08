module Interpreter.Builtins.BuiltinFunctions

open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
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
    Function((fun _ v -> Function((fun _ v' -> Application(v', v)))))

let listConsOp = Function((fun _ v -> Function((fun _ -> node' v))))
