module Interpreter.Builtins.BuiltinFunctions

open Interpreter.Builtins.BuiltinRefs
open Interpreter.Main
open Interpreter.Value

let andOp =
    Function(
        (fun st v ->
            if not (checkCondition st v) then
                false' ()
            else
                Function (fun st' v' -> if (checkCondition st' v') then true' () else false' ()))

    )

let orOp =
    Function(
        (fun st v ->
            if (checkCondition st v) then
                true' ()
            else
                Function (fun st' v' -> if (checkCondition st' v') then true' () else false' ()) )
    )

let applicationOp =
    Function (fun _ v -> Function (fun _ v' -> Application(v, v')))

let reverseApplicationOp =
    Function((fun _ v -> Function((fun _ v' -> Application(v', v)))))

let listConsOp = Function((fun _ v -> Function((fun _ -> node' v))))
