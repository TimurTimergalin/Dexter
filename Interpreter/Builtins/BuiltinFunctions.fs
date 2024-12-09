module Interpreter.Builtins.BuiltinFunctions

open System
open Interpreter
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
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

let truth =
    Function(
        fun st v ->
            let (Type(_, ns)) = getType st v
            let truthMember = Ref(ns, topLevelGetter "truth", topLevelSetter "truth")
            Application(truthMember, v)
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

let input =
    Function(
        fun _ _ ->
            Action(
                fun () ->
                    let str = System.Console.ReadLine()
                    String str
            )
    )

let int' =
    Function(
        fun st v ->
            let ev = dereference st v
            let Type(name, _) as type' = getType st v
            if typeEq type' intRef.Value then
                v
            elif typeEq type' floatRef.Value then
                let (Float f) = ev
                Int(f |> int)
            elif typeEq type' stringRef.Value then
                let (String s) = ev
                try
                    Int(s |> int)
                with :? FormatException -> raise (parseError "int")
            elif typeEq type' boolRef.Value then
                let (Object(Constructor(name', _, _), _)) = ev
                let (Constructor(name'', _, _)) = trueRef.Value
                if name' = name'' then Int(1) else Int(0) 
            else
                raise (conversionError name "Int")
    )

let float' =
    Function(
        fun st v ->
            let ev = dereference st v
            let Type(name, _) as type' = getType st v
            if typeEq type' floatRef.Value then
                v
            elif typeEq type' intRef.Value then
                let (Int i) = ev
                Float(i |> float)
            elif typeEq type' stringRef.Value then
                let (String s) = ev
                try
                    Float(s |> float)
                with :? FormatException -> raise (parseError "float")
            elif typeEq type' boolRef.Value then
                let (Object(Constructor(name', _, _), _)) = ev
                let (Constructor(name'', _, _)) = trueRef.Value
                if name' = name'' then Float(1.0) else Float(0.0)
            else
                raise (conversionError name "float")
    )
