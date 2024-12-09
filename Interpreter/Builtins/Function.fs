module Interpreter.Builtins.Function

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Value

let functionMembers: Context = Dictionary() |> applyDefaultNegation |> applyDefaultEquality |> applyDefaultInequality
let functionType = Type("Function", functionMembers)

let isFunction stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Function _ -> true' ()
    | _ -> false' ()

functionMembers.Add("inst", Val(Function(isFunction)))
let functionTruth st v =
    let ev = dereference st v
    match ev with
    | Function _ -> true'()
    | _ -> raise (unexpectedType "Function.truth")
functionMembers.Add("truth", Val(Function(functionTruth)))

let functionRepr =
    Function(
        fun st v ->
            let ev = dereference st v
            match ev with
            | Function _ -> String "Function"
            | _ -> raise (unexpectedType "Function.repr")
    )
functionMembers.Add("repr", Val functionRepr)

let reverseComposition =
    Function(fun _ v -> Function(fun _ v' -> Function(fun _ v'' -> Application(v', Application(v, v'')))))
functionMembers.Add("(>>)", Val(reverseComposition))

let composition =
    Function(fun _ v -> Function(fun _ v' -> Function(fun _ v'' -> Application(v, Application(v', v'')))))
functionMembers.Add("(<<)", Val(reverseComposition))
