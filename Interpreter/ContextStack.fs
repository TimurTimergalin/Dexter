﻿module Interpreter.ContextStack

open Interpreter.Exceptions
open Interpreter.Imperative
open Interpreter.Value
open Microsoft.FSharp.Collections
open Parser.Ast

let lastName name =
    match name with
            | Identifier s -> s
            | Operator(s, opt) ->
                let qm = if opt then "?" else ""
                "(" + s + qm + ")"

let fullName = function NamespacedName(ns, name) -> (String.concat "." ns) + lastName name
                

let rec getRef (stack: ContextStack) (NamespacedName(ns, name) as nsName) =
    let fn = fullName nsName
    match stack with
    | Empty -> raise (nameError fn)                                
    | Has(head, tail) ->
        if not (head.ContainsKey(List.head ns)) then
            getRef tail nsName
        else
            let folder = (fun (ctx: Context) cur ->
                               if not (ctx.ContainsKey cur) then
                                   raise (nameError fn)
                               match ctx[cur] with
                               | Val _ -> raise (nameError fn)
                               | Namespace ctx' -> ctx')
            let finalCtx = List.fold folder  head ns
            let ln = lastName name
            if not (finalCtx.ContainsKey(ln)) then
                raise (nameError fn)
            match finalCtx[ln] with
            | Namespace _ -> raise (nameError fn)
            | x -> Ref(ln, finalCtx)
                            