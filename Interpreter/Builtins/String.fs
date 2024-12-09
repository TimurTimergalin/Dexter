﻿module Interpreter.Builtins.String

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Value

let stringMembers: Context = Dictionary() |> applyDefaultInequality |> applyDefaultNegation
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
    | String _ -> true' ()
    | _ -> raise (unexpectedType "String.truth")
stringMembers.Add("truth", Val(Function(stringTruth)))


let stringRepr =
    Function(
        fun st v ->
            if typeEq stringRef.Value (getType st v) then
                v
            else
                raise (unexpectedType "String.repr")
    )
stringMembers.Add("repr", Val stringRepr)

let stringAdd =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | String s ->
                        match ev' with
                        | String s' -> String(s + s')
                        | _ -> raise (unexpectedType "String.(+)")
                    | _ -> raise (unexpectedType "String.(+)")
            )
    )
stringMembers.Add("(+)", Val stringAdd)

let stringEq =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | String s ->
                        match ev' with
                        | String s' -> if s = s' then true'() else false'()
                        | _ -> raise (unexpectedType "String.(=)")
                    | _ -> raise (unexpectedType "String.(=)")
            )
    )
stringMembers.Add("(=)", Val stringEq)

let stringLt =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | String s ->
                        match ev' with
                        | String s' -> if s < s' then true'() else false'()
                        | _ -> raise (unexpectedType "String.(<)")
                    | _ -> raise (unexpectedType "String.(<)")
            )
    )
stringMembers.Add("(<)", Val stringLt)

let stringLe =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | String s ->
                        match ev' with
                        | String s' -> if s <= s' then true'() else false'()
                        | _ -> raise (unexpectedType "String.(<=)")
                    | _ -> raise (unexpectedType "String.(<=)")
            )
    )
stringMembers.Add("(<=)", Val stringLe)

let stringGt =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | String s ->
                        match ev' with
                        | String s' -> if s > s' then true'() else false'()
                        | _ -> raise (unexpectedType "String.(>)")
                    | _ -> raise (unexpectedType "String.(>)")
            )
    )
stringMembers.Add("(>)", Val stringGt)

let stringGe =
    Function(
        fun st v ->
            Function(
                fun st' v' ->
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | String s ->
                        match ev' with
                        | String s' -> if s >= s' then true'() else false'()
                        | _ -> raise (unexpectedType "String.(>=)")
                    | _ -> raise (unexpectedType "String.(>=)")
            )
    )
stringMembers.Add("(>=)", Val stringGe)

