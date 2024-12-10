module Interpreter.RefUtil

open Interpreter.Exceptions
open Interpreter.Imperative
open Interpreter.Value
open Microsoft.FSharp.Collections
open Parser.Ast

let topLevelGetter str (ctx: Context) =
    let (Val value) = ctx[str]
    value

let topLevelSetter str (ctx: Context) value = withSet ctx str (Val value)

let lastName name =
    match name with
    | Identifier s -> s
    | Operator(s, _) -> "(" + s + ")"

let fullName =
    function
    | NamespacedName(ns, name) -> (String.concat "." ns) + (if ns <> [] then "." else "") + lastName name

let rec getRef (stack: ContextStack) (NamespacedName(ns, name) as nsName) =
    let fn = fullName nsName
    let ln = lastName name

    match stack with
    | [] -> raise (unknownNameError fn)
    | head :: tail ->
        if
            (ns = [] && not (head.ContainsKey ln))
            || (ns <> [] && not (head.ContainsKey(List.head ns)))
        then
            getRef tail nsName
        else
            let folder =
                (fun (ctx: Context) cur ->
                    if not (ctx.ContainsKey cur) then
                        raise (unknownNameError fn)

                    match ctx[cur] with
                    | Val _ -> raise (unknownNameError fn)
                    | Namespace ctx' -> ctx')

            let finalCtx = List.fold folder head ns

            if not (finalCtx.ContainsKey(ln)) then
                raise (unknownNameError fn)

            match finalCtx[ln] with
            | Namespace _ -> raise (unknownNameError fn)
            | _ -> Ref(finalCtx, topLevelGetter ln, topLevelSetter ln)

let getRefSafe stack nsName =
    try
        Result.Ok(getRef stack nsName)
    with :? DexterError ->
        Result.Error()
