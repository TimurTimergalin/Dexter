module Interpreter.Builtins

open System.Collections.Generic
open Interpreter.BuiltinRefs
open Interpreter.Main
open Interpreter.Value

let noneTypeMembers: Context = Dictionary()
let noneType = Type("NoneType", noneTypeMembers)
let noneCons = Constructor("None", 0, noneType)
let isNone stack v =
    let evaluated = evalUntilRecognizable stack v
    objIsInst noneType evaluated
noneTypeMembers.Add("inst", Val (Function isNone))

let boolMembers: Context = Dictionary()
let boolType = Type("Bool", boolMembers) 
let trueCons = Constructor("True", 0, boolType)
let falseCons = Constructor("False", 0, boolType)
let isBool stack (v: Value) =
    let evaluated = evalUntilRecognizable stack v
    objIsInst boolType evaluated
boolMembers.Add("inst", Val (Function isBool))

let listMembers: Context = Dictionary()
let listType = Type("List", listMembers)
let nodeCons = Constructor("Node", 2, listType)
let endCons = Constructor("End", 0, listType)
let isList stack v =
    let evaluated = evalUntilRecognizable stack v
    objIsInst listType evaluated
listMembers.Add("inst", Val (Function isList))
    

let builtins: Context =
    let res = Dictionary()
    res.Add("None", Val noneCons)
    res.Add("NoneType", Namespace noneTypeMembers)
    
    res.Add("True", Val trueCons)
    res.Add("False", Val falseCons)
    res.Add("Bool", Namespace boolMembers)
    
    res.Add("Node", Val nodeCons)
    res.Add("End", Val endCons)
    res.Add("List", Namespace listMembers)
    res


// В отличие от сишных заголовков, наши "заголовки" никакой линкер заполнять не будет -
// все нужно делать вручную...
 
noneRef.Value <- Ref("None", builtins)
noneTypeRef.Value <- noneType

trueRef.Value <- Ref("True", builtins)
falseRef.Value <- Ref("False", builtins)
boolRef.Value <- boolType

nodeRef.Value <- Ref("Node", builtins)
endRef.Value <- Ref("End", builtins)
listRef.Value <- listType


