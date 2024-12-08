module Interpreter.Types

open Interpreter.Value

type InterpreterParams =
    { run: bool
      module': string
      stdPath: string
      libPath: string }

type InterpreterResults =
    { context: Context
      result: Value option }
