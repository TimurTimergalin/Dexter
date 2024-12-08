module Interpreter.Builtins.String

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Value

let stringMembers: Context = Dictionary()
let stringType = Type("String", stringMembers)

let isString stack v =
    let evaluated = dereference stack v

    match evaluated with
    | String _ -> true' ()
    | _ -> false' ()

stringMembers.Add("inst", Val(Function(isString)))

let stringTruth stack v =
    let evaluated = dereference stack v

    match evaluated with
    | String "" -> false' ()
    | _ -> true' ()

stringMembers.Add("truth", Val(Function(stringTruth)))
