module Interpreter.Types

open Interpreter.Value

type InterpreterParams = {
    run: bool;
    module': string 
}

type InterpreterResults = {
    context: Context
    result: Value option
}