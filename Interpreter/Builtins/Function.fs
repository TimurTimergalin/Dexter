﻿module Interpreter.Builtins.Function

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Main
open Interpreter.Value

let functionMembers: Context = Dictionary()
let functionType = Type("Function", functionMembers)

let isFunction stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Function _ -> true' ()
    | _ -> false' ()

functionMembers.Add("inst", Val(Function(isFunction)))
let functionTruth _ _ = true' ()
functionMembers.Add("truth", Val(Function(functionTruth)))

let reverseComposition =
    Function(
        fun _ v -> Function(
            fun _ v' -> Function(
                fun _ v'' -> Application(v', Application(v, v''))
            )
        )
    )
functionMembers.Add("(>>)", Val(reverseComposition))
