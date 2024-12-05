module Interpreter.Main

open Interpreter.ContextStack
open Interpreter.Exceptions
open Interpreter.Imperative
open Interpreter.Value
open Parser.Ast

let rec recognizable = function
    | Unrecognizable _ -> false
    | Ref(name, ctx) ->
        let (Val value) = ctx[name]
        recognizable value
    | _ -> true

// Эта функция будет применять некоторые редукции к выражениям.
// Данная функция пытается посчитать как можно меньше - за счет этого достигается ленивость.
// Результат функции не обязательно будет 
let rec eval1 (stack: ContextStack) (value: Value): Value =
    match value with
    | Unrecognizable node ->
        match node with
        | StringLiteral s -> String s
        | IntLiteral i -> Int i
        | FloatLiteral f -> Float f
        | NamespacedName _ as nsName -> getRef stack nsName
        | _ -> failwith "Not implemented"
            
    | x -> x

and evalUntilRecognizable (stack: ContextStack) value =
    if recognizable value then
        value
    else
        evalUntilRecognizable stack (eval1 stack value)

let rec evalAll (stack: ContextStack) = function
    | Object(Constructor(name, argsCount, Type(tn, _)) as cons, args) ->
        let givenArgsCount = List.length args
        if givenArgsCount <> argsCount then
            raise (constructorError name tn argsCount givenArgsCount)
        let newArgs = List.map (evalAll stack) args
        Object(cons, newArgs)
    | Ref(key, ctx) ->
        let (Val value) = ctx[key]
        // Использование withSet формально порождает побочные эффекты,
        // но логическая константность словаря ctx не нарушена - по ключу лежит то же значение, что и раньше,
        // только вычисленное
        Ref(key, withSet ctx key (Val(evalAll stack value)))
    | Unrecognizable _ as nn -> evalAll stack (evalUntilRecognizable stack nn)
    | x -> x


