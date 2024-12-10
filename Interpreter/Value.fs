module Interpreter.Value

open System.Collections.Generic
open Parser.Ast

type Value =
    | Float of float
    | Int of int
    | String of string
    | Function of (ContextStack -> Value -> Value)
    | Action of (unit -> Value)
    | Constructor of string * int * Type'
    | Object of Value * List<Value>
    | Ref of Context * (Context -> Value) * (Context -> Value -> Context)
    | Unrecognizable of Node
    | Closure of Value * ContextStack * bool
    | Application of Value * Value
    | FlatApplication of Value * Value list
    | ApplyAfter of Value * ContextStack * (Value -> bool) * (Value -> Value) list * ContextStack list * (Value -> bool) list
    with override this.ToString() =
            match this with
            | Object(cons, args) -> sprintf $"Object({cons}, SubObjects[%d{args.Count}])"
            | Ref(ctx, getter, _) -> sprintf $"Ref(RefContext[%d{ctx.Count}], {getter ctx})"
            | Closure(value, stack, isolated) -> sprintf $"Closure({value}, closureStack[%d{stack.Length}], %b{isolated})"
            | Application(v, v') -> $"Application({v}, {v'})"
            | FlatApplication(f, args) -> sprintf $"FlatApplication({f}, flatAppArgs[%d{args.Length}])"
            | ApplyAfter(v, st, _, app, stacks, conds) -> sprintf $"ApplyAfter({v}, ApplyAfterStack[%d{st.Length}], ..., ApplyAfterFuncs[%d{app.Length}], stacks[%d{stacks.Length}], conds[%d{conds.Length}])"
            | _ -> sprintf $"%A{this}"

and Context = Dictionary<string, ContextEntry>
and Type' = Type of string * Context | Absent

and ContextEntry =
    | Val of Value
    | Namespace of Context

and ContextStack = Context list

let defaultForceStop _ = false
