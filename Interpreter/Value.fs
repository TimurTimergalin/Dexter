module Interpreter.Value

open System.Collections.Generic
open Parser
open Parser.Ast

type Value =
    | Float of float
    | Int of int
    | String of string
    | Function of (ContextStack -> Value -> Value) * bool
    | Action of (unit -> Value)
    | Constructor of string * int * Value
    | Type of string * Context
    | Object of Value * List<Value>
    | Ref of Context * (Context -> Value) * (Context -> Value -> Context)
    | Unrecognizable of Node
    | Closure of Value * ContextStack * bool
and Context = Dictionary<string, ContextEntry>
and ContextEntry =
    | Val of Value
    | Namespace of Context
and ContextStack = Context list
