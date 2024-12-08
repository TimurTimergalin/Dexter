module Interpreter.Builtins.Int

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Exceptions
open Interpreter.Main
open Interpreter.Value

let intMembers: Context = Dictionary()
let intType = Type("Int", intMembers)

let isInt stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Int _ -> true' ()
    | _ -> false' ()

intMembers.Add("inst", Val(Function(isInt)))

let intTruth stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Int(0) -> false' ()
    | _ -> true' ()

intMembers.Add("truth", Val(Function(intTruth)))

let addition =
    Function(
        fun st v -> Function(
            fun st' v' ->
                let ev = dereference st v
                let ev' = dereference st' v'
                match ev with
                | Int x ->
                    match ev' with
                    | Int y -> Int (x + y)
                    | _ -> raise (TypeError "Invalid type of arguments for function '(+)'")
                | _ -> raise (TypeError "Invalid type of arguments for function '(+)'")
        )
    )
intMembers.Add("(+)", Val addition)
