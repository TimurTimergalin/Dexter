module Interpreter.Value

open System.Collections.Generic
open Parser.Ast

type Value =
    | Float of float
    | Int of int
    | String of string
    | Function of (Value -> Value)
    | Action of (unit -> unit)
    | Constructor of string * int * Value
    | Type of string * Map<string, Value>
    | Object of Value * Value list
    | Ref of string * Context
    | Unrecognizable of Node
and Context = Dictionary<string, ContextEntry>
and ContextEntry =
    | Val of Value
    | Namespace of Context
    
