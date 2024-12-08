module Interpreter.Builtins.FLoat

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Value

let floatMembers: Context = Dictionary()
let floatType = Type("Float", floatMembers)

let isFloat stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Float _ -> true' ()
    | _ -> false' ()

floatMembers.Add("inst", Val(Function(isFloat)))

let floatTruth stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Float(0.) -> false' ()
    | _ -> true' ()

floatMembers.Add("truth", Val(Function(floatTruth)))
