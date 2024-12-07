module Interpreter.Main

open System.Collections.Generic
open Interpreter.BuiltinRefs
open Interpreter.RefUtil
open Interpreter.Exceptions
open Interpreter.Imperative
open Interpreter.Value
open Parser.Ast

let typeEq (Type(_, ns1)) (Type(_, ns2)) = ns1 = ns2

let objIsInst type' obj =
    match obj with
    | Object(Constructor(_, _, type''), _) when typeEq type' type'' -> true' ()
    | _ -> false' ()

// Выражение будем называть узнаваемым, если у него можно определить тип, не применяя редукций
let rec recognizable =
    function
    | Unrecognizable _ -> false
    | Closure _ -> false
    | Ref(ctx, getter, _) -> recognizable (getter ctx)
    | Action _ -> false
    | _ -> true

let constructorToObject (Constructor(_, argsCount, _) as cons) =
    let rec construct vars varsRem =
        match varsRem with
        | [] -> Object(cons, reversed vars)
        | cur :: rest ->
            Value.Function(
                (fun _ v ->
                    let ctx = withSet (Dictionary()) cur (Val v)
                    let ref' = Ref(ctx, topLevelGetter cur, topLevelSetter cur)
                    Closure(construct (withAdded vars ref') rest, [ ctx ], false)),
                false
            )

    let vars = seq { for i in 1..argsCount -> $"a%d{i}" } |> Seq.toList
    construct (List()) vars

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
        | Application(f, arg) ->
            let evf = dereference stack (Unrecognizable f)

            match evf with
            | Value.Function(f', _) -> f' stack (Unrecognizable arg)
            | _ -> raise notCallableError

        | _ -> failwith "Not implemented"
    | Closure(value', stack', isolated) ->
        match value' with
        | Closure(value'', stack'', false) -> Closure(value'', List.concat [ stack''; stack' ], isolated)
        | Object(cons, args) -> Object(cons, map (fun v -> Closure(v, stack', isolated)) args)
        | Value.Function(f, true) -> Value.Function((fun st v -> f st (Closure(v, stack', isolated))), true)
        | Value.Function(f, false) -> Value.Function((fun st v -> Closure(f st v, stack', isolated)), false)
        | x when not (recognizable x) -> Closure(eval1 stack' x, stack', isolated)
        | x -> x
    | Ref(ctx, getter, setter) ->
        let ctx' = setter ctx (eval1 stack (getter ctx))
        Ref(ctx', getter, setter)
    | Action f -> f ()
    | x -> x

and evalUntilRecognizable (stack: ContextStack) (value: Value) =
    if recognizable value then
        value
    else
        evalUntilRecognizable stack (eval1 stack value)

and dereference stack value =
    let evaluated = evalUntilRecognizable stack value

    match evaluated with
    | Ref(ctx, getter, setter) ->
        match getter ctx with
        | Object(cons, args) ->
            let objSetter i value' = Object(cons, withUpdated args i value')

            Object(
                cons,
                args
                |> mapi (fun i _ -> Ref(ctx, (fun _ -> args[i]), (fun ctx' value' -> setter ctx' (objSetter i value'))))
            )
        | Ref _ as inner -> dereference stack inner
        | Constructor _ as cons -> constructorToObject cons
        | Value.Function(f, _) -> Value.Function(f, true)
        | Closure(v, st, _) -> Closure(v, st, true)
        | x -> x
    | _ -> evaluated

and satisfiesConstructor (stack: ContextStack) (Constructor(name, _, type')) (value: Value) : bool =
    let evaluated = dereference stack value

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
        | Ok(Ref(ctx, getter, _)) ->
            match getter ctx with
            | Constructor(cn, argsCount, Type(tn, _)) as cons ->
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
        let (Ref(ctx, getter, _)) = getRef stack nsName

        match getter ctx with
        | Constructor _ as cons -> compileConstructor stack cons args value
    | LiteralPattern literal ->
        let evaluated = dereference stack value

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
            let evaluated = dereference stack value

            match evaluated with
            | Object(Constructor("Empty", 0, type'), _) when typeEq type' listRef.Value -> Some(Dictionary())
            | _ -> None
        | head :: tail -> compileConstructor stack nodeRef.Value [ head; ListPattern tail ] value
    | HeadTailPattern(head, tail) -> compileConstructor stack nodeRef.Value [ head; tail ] value
    | _ -> failwith "Trying to compile not-a-pattern"

let rec evalAll (stack: ContextStack) =
    function
    | Object(Constructor(name, argsCount, Type(tn, _)) as cons, args) ->
        let givenArgsCount = args.Count

        if givenArgsCount <> argsCount then
            raise (constructorError name tn argsCount givenArgsCount)

        let newArgs = map (evalAll stack) args
        Object(cons, newArgs)
    | Ref(ctx, getter, setter) ->
        let ctx' = setter ctx (evalAll stack (getter ctx))
        Ref(ctx', getter, setter)
    | Unrecognizable _ as nn -> evalAll stack (evalUntilRecognizable stack nn)
    | x -> x
