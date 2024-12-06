module Interpreter.Main

open System.Collections.Generic
open Interpreter.BuiltinRefs
open Interpreter.ContextStack
open Interpreter.Exceptions
open Interpreter.Imperative
open Interpreter.Value
open Parser.Ast

let typeEq (Type(_, ns1)) (Type(_, ns2)) = ns1 = ns2

let objIsInst type' obj =
    let (Type(name, members)) = type'

    match obj with
    | Object(Constructor(_, _, type''), _) when typeEq type' type'' -> true' ()
    | _ -> false' ()

// Выражение будем называть узнаваемым, если у него можно определить тип, не применяя редукций
let rec recognizable =
    function
    | Unrecognizable _ -> false
    | Closure _ -> false
    | Ref(name, ctx) ->
        let (Val value) = ctx[name]
        recognizable value
    | Action _ -> false
    | _ -> true

// Эта функция будет применять некоторые редукции к выражениям.
// Данная функция пытается посчитать как можно меньше - за счет этого достигается ленивость.
// Результат функции не обязательно будет "узнаваемым"
let rec eval1 (stack: ContextStack) (value: Value) : Value =
    match value with
    | Unrecognizable node ->
        match node with
        | StringLiteral s -> String s
        | IntLiteral i -> Int i
        | FloatLiteral f -> Float f
        | NamespacedName _ as nsName -> getRef stack nsName
        | _ -> failwith "Not implemented"
    | Closure(value', stack') ->
        let result = withExtended stack stack' <| fun st -> eval1 st value'

        match result with
        | Closure(value'', stack'') -> Closure(value'', concatenated stack'' stack')
        | Object(cons, args) -> Object(cons, FSharp.Collections.List.map (fun x -> Closure(x, stack')) args)
        | Value.Function f ->
            let newF stack'' = withExtended stack'' stack' f
            Value.Function newF
        | x when recognizable x -> x
        | x -> Closure(x, stack')
    | Action f -> f ()
    | x -> x

and evalUntilRecognizable (stack: ContextStack) value =
    if recognizable value then
        value
    else
        evalUntilRecognizable stack (eval1 stack value)

and satisfiesConstructor (stack: ContextStack) (Constructor(name, _, type')) (value: Value) : bool =
    let evaluated = evalUntilRecognizable stack value

    match evaluated with
    | Object(Constructor(name', _, type''), _) when name = name' && typeEq type' type'' -> true
    | _ -> false

and compileConstructor
    (stack: ContextStack)
    (Constructor(cn, argsCount, Type(tn, _)) as cons)
    args
    (value: Value)
    : Context option =
    let given = FSharp.Collections.List.length args

    if given <> argsCount then
        raise (constructorError cn tn argsCount given)

    if not (satisfiesConstructor stack cons value) then
        None
    else
        let (Object(_, args')) = value
        let subtasks = Seq.zip args args'

        let results =
            subtasks |> Seq.map (fun (n, v) -> compilePattern stack n v) |> Seq.toList

        if not (List.forall Option.isSome results) then
            None
        else
            let someResults = results |> List.map Option.get
            let ctx' = combine someResults

            match ctx' with
            | Ok ctx'' -> Some ctx''
            | Error name -> raise (incorrectNameBinding name)

and compilePattern (stack: ContextStack) (node: Node) (value: Value) : Context option =
    match node with
    | SkipPattern -> Some(Dictionary())
    | NameBind nsName ->
        let ref' = getRefSafe stack nsName

        match ref' with
        | Ok(Ref(name, ctx)) ->
            match ctx[name] with
            | Val(Constructor(cn, argsCount, Type(tn, _)) as cons) ->
                if argsCount <> 0 then
                    raise (constructorError cn tn argsCount 0)

                if satisfiesConstructor stack cons value then
                    Some(Dictionary())
                else
                    None
            | _ ->
                let (NamespacedName(_, Identifier(name'))) = nsName
                Some(withSet (Dictionary()) name' (Val value))
        | _ ->
            let (NamespacedName(_, Identifier(name'))) = nsName
            Some(withSet (Dictionary()) name' (Val value))
    | ConstructorPattern(nsName, args) ->
        let (Ref(name, ctx)) = getRef stack nsName

        match ctx[name] with
        | Val(Constructor _ as cons) ->
            compileConstructor stack cons args value
    | LiteralPattern literal ->
        let evaluated = evalUntilRecognizable stack value

        match literal with
        | StringLiteral s ->
            match evaluated with
            | String s' when s' = s -> Some(Dictionary())
            | _ -> None
        | IntLiteral i ->
            match evaluated with
            | Int i' when i = i' -> Some(Dictionary())
            | Float f when float i = f -> Some(Dictionary())
            | _ -> None
        | FloatLiteral f ->
            match evaluated with
            | Float f' when f = f' -> Some(Dictionary())
            | Int i when f = float i -> Some(Dictionary())
            | _ -> None
        | _ -> failwith "Unreachable: unknown literal"
    | ListPattern nodes ->
        match nodes with
        | [] ->
            let evaluated = evalUntilRecognizable stack value

            match evaluated with
            | Object(Constructor("Empty", 0, type'), _) when typeEq type' listRef.Value -> Some(Dictionary())
            | _ -> None
        | head :: tail -> compileConstructor stack nodeRef.Value [head; ListPattern tail] value
    | HeadTailPattern(head, tail) ->
        compileConstructor stack nodeRef.Value [head; tail] value
    | _ -> failwith "Trying to compile not-a-pattern"

let rec evalAll (stack: ContextStack) =
    function
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
