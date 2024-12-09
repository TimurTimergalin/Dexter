module Interpreter.Builtins.Builtins

open System.Collections.Generic
open Interpreter.Builtins.Bool
open Interpreter.Builtins.BuiltinFunctions
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Builtins.FLoat
open Interpreter.Builtins.Function
open Interpreter.Builtins.Int
open Interpreter.Builtins.List
open Interpreter.Builtins.NoneType
open Interpreter.Builtins.String
open Interpreter.RefUtil
open Interpreter.Value

let builtins: Context =
    let res = Dictionary()
    
    let mixInBuiltins = fun x -> Closure(x, [res], false)
    
    res.Add("None", Val noneCons)
    res.Add("NoneType", Namespace noneTypeMembers)

    res.Add("True", Val trueCons)
    res.Add("False", Val falseCons)
    res.Add("Bool", Namespace boolMembers)

    res.Add("Node", Val nodeCons)
    res.Add("End", Val endCons)
    res.Add("List", Namespace listMembers)

    res.Add("Int", Namespace intMembers)
    res.Add("Float", Namespace floatMembers)
    res.Add("String", Namespace stringMembers)
    res.Add("Function", Namespace functionMembers)

    res.Add("(&&)", Val (mixInBuiltins andOp))
    res.Add("(||)", Val (mixInBuiltins orOp))
    res.Add("(<|)", Val (mixInBuiltins applicationOp))
    res.Add("(|>)", Val (mixInBuiltins reverseApplicationOp))
    res.Add("(::)", Val (mixInBuiltins listConsOp))
    
    res.Add("repr", Val (mixInBuiltins repr))
    res.Add("print", Val (mixInBuiltins print))
    res.Add("truth", Val (mixInBuiltins truth))
    res.Add("input", Val (mixInBuiltins input))
    res.Add("int", Val (mixInBuiltins int'))
    res.Add("float", Val (mixInBuiltins float'))
    res.Add("fail", Val (mixInBuiltins fail))

    res


// В отличие от сишных заголовков, наши "заголовки" никакой линкер заполнять не будет -
// все нужно делать вручную...

let builtinsInit () =
    noneRef.Value <- Ref(builtins, topLevelGetter "None", topLevelSetter "None")
    noneTypeRef.Value <- noneType

    trueRef.Value <- Ref(builtins, topLevelGetter "True", topLevelSetter "True")
    falseRef.Value <- Ref(builtins, topLevelGetter "False", topLevelSetter "False")
    boolRef.Value <- boolType

    nodeRef.Value <- Ref(builtins, topLevelGetter "Node", topLevelSetter "Node")
    endRef.Value <- Ref(builtins, topLevelGetter "End", topLevelSetter "End")
    listRef.Value <- listType

    intRef.Value <- intType
    floatRef.Value <- floatType
    stringRef.Value <- stringType
    functionRef.Value <- functionType
    
    reprRef.Value <- Ref(builtins, topLevelGetter "repr", topLevelSetter "repr")
    printRef.Value <- Ref(builtins, topLevelGetter "print", topLevelSetter "print")
    truthRef.Value <-Ref(builtins, topLevelGetter "truth", topLevelSetter "truth")
    inputRef.Value <- Ref(builtins, topLevelGetter "input", topLevelSetter "input")
    int'Ref.Value <- Ref(builtins, topLevelGetter "int", topLevelSetter "int")
    float'Ref.Value <- Ref(builtins, topLevelGetter "float", topLevelSetter "float")
