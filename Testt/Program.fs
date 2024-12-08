open FParsec
open Interpreter.Builtins.Builtins
open Interpreter.Main
open Interpreter.Value
open Parser.Main

[<EntryPoint>]
let main argv =
    builtinsInit()
    let parser = expression .>> eof
    let input = "None |>? fun x -> x + 2"
    let value =
        match run parser input with
        | Success(node, _, _) -> node
        | Failure(s, _, _) -> failwith s
    let stack = [builtins]
    let result = dereference stack (evalAll stack (Unrecognizable value))
    let toPrint = result
    printf $"{toPrint}"
    0 // return an integer exit code
