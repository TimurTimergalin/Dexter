module Interpreter.Value

open System.Collections.Generic
open System.Diagnostics
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

and Context = Dictionary<string, ContextEntry>
and Type' = Type of string * Context | Absent

and ContextEntry =
    | Val of Value
    | Namespace of Context

and ContextStack = Context list
