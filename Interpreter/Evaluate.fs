module Interpreter.Evaluate

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
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
    | Value.Application _ -> false
    | _ -> true

let constructorToObject (Constructor(_, argsCount, _) as cons) =
    let rec construct vars varsRem =
        match varsRem with
        | [] -> Object(cons, reversed vars)
        | cur :: rest ->
            Value.Function(fun _ v ->
                let ctx = withSet (Dictionary()) cur (Val v)
                let ref' = Ref(ctx, topLevelGetter cur, topLevelSetter cur)
                Closure(construct (withAdded vars ref') rest, [ ctx ], false))

    let vars = seq { for i in 1..argsCount -> $"a%d{i}" } |> Seq.toList
    construct (List()) vars

// Эта функция будет применять некоторые редукции к выражениям.
// Данная функция пытается посчитать как можно меньше - за счет этого достигается ленивость.
// Результат функции не обязательно будет узнаваемым
let rec eval1 (stack: ContextStack) (value: Value) : Value =
    match value with
    | Unrecognizable node ->
        match node with
        | StringLiteral s -> String s
        | IntLiteral i -> Int i
        | FloatLiteral f -> Float f
        | NamespacedName(ns, name) as nsName ->
            match name with
            | Identifier _ -> getRef stack nsName
            | Operator(name', opt) ->
                let nonOptionalOperator =
                    if ns <> [] then
                        getRef stack nsName
                    else
                        match getRefSafe stack nsName with
                        | Ok op -> op
                        | Error _ -> Value.Function(fun st v -> Value.Application(defaultOperator st name' v, v))

                if not opt then
                    nonOptionalOperator
                else
                    optionalOperator nonOptionalOperator

        | Application(f, arg) -> Value.Application(Unrecognizable f, Unrecognizable arg)
        | Conditional(cond, trueBranch, falseBranch) ->
            if checkCondition stack (Unrecognizable cond) then
                Unrecognizable trueBranch
            else
                Unrecognizable falseBranch
        | ListLiteral nodes ->
            match nodes with
            | [] -> end' ()
            | head :: tail -> node' (Unrecognizable head) (Unrecognizable(ListLiteral tail))
        | Function(arg, body) ->
            let pattern = compilePattern stack arg

            let res _ v =
                let clr = pattern v

                match clr with
                | None -> raise patternFailed
                | Some ctx -> Closure(Unrecognizable body, [ ctx ], false)

            Value.Function res
        | Match(valueNode, cases) ->
            let tempCtx = withSet (Dictionary()) "temp" (Val(Unrecognizable valueNode))
            let value = Ref(tempCtx, topLevelGetter "temp", topLevelSetter "temp")

            let checkCase (Case(pattern, cond, body)) =
                let matchResult = compilePattern stack pattern value

                match matchResult with
                | None -> None
                | Some ctx ->
                    let res = Some(Closure(Unrecognizable body, [ ctx ], false))

                    match cond with
                    | None -> res
                    | Some condExpr ->
                        if checkCondition (ctx :: stack) (Unrecognizable condExpr) then
                            res
                        else
                            None

            let rec chooseCase =
                function
                | [] -> raise patternFailed
                | case :: rest ->
                    match checkCase case with
                    | None -> chooseCase rest
                    | Some res -> res

            chooseCase cases
        | Compound stmts ->
            let ctx: Context = Dictionary()

            let finalCtx, last =
                FSharp.Collections.List.fold
                    (fun (ctx', _) stmt ->
                        match stmt with
                        | Equation _ -> (performEquation stack ctx' stmt, none' ())
                        | Eval _ -> (performEval stack ctx' stmt, none' ())
                        | TypeDeclaration _ -> (performType stack ctx' stmt, none' ())
                        | expr -> (ctx', Unrecognizable expr))
                    (ctx, none' ())
                    stmts

            Closure(last, [ finalCtx ], false)
        | _ -> failwith "Not implemented"
    | Closure(value', stack', isolated) ->
        match value' with
        | Closure(value'', stack'', false) -> Closure(value'', List.concat [ stack''; stack' ], isolated)
        | Object(cons, args) -> Object(cons, map (fun v -> Closure(v, stack', isolated)) args)
        | Value.Function(f) -> Value.Function(fun st v -> Closure(f st v, stack', isolated))
        | x when not (recognizable x) -> Closure(eval1 stack' x, stack', isolated)
        | x -> x
    | Ref(ctx, getter, setter) ->
        let ctx' = setter ctx (eval1 [] (getter ctx))
        Ref(ctx', getter, setter)
    | Value.Application(f, arg) ->
        let evf = dereference stack f

        match evf with
        | Value.Function(f') -> f' stack arg
        | _ -> raise notCallableError
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
            let newSetter i ctx' value' =
                let (Object(_, args')) = getter ctx'
                setter ctx' (Object(cons, withUpdated args' i value'))

            let newGetter i ctx' =
                let (Object(_, args')) = getter ctx'
                args'[i]

            Object(cons, List(args) |> mapi (fun i _ -> Ref(ctx, newGetter i, newSetter i)))
        | Ref _ as inner -> dereference stack inner
        | Constructor _ as cons -> constructorToObject cons
        | Value.Function f -> Value.Function(fun st v -> Closure(f st v, [], true))
        | Closure(v, st, _) -> Closure(v, st, true)
        | x -> x
    | _ -> evaluated

and getType (stack: ContextStack) v =
    let evaluated = dereference stack v

    match evaluated with
    | Int _ -> intRef.Value
    | Float _ -> floatRef.Value
    | String _ -> stringRef.Value
    | Value.Function _ -> functionRef.Value
    | Object(Constructor(_, _, type'), _) -> type'

and getMember (Type(tn, members)) name =
    if members.ContainsKey name then
        Ref(members, topLevelGetter name, topLevelSetter name)
    else
        raise (noMemberError tn name)

and checkCondition (stack: ContextStack) (cond: Value) =
    let evaluated = dereference stack cond
    let truthFunc = dereference stack (getMember (getType stack evaluated) "truth")

    let truthValue =
        match truthFunc with
        | Value.Function(f) -> f stack evaluated
        | _ -> raise notCallableError

    let truthValueEvaluated = dereference stack truthValue

    match truthValueEvaluated with
    | x when satisfiesConstructor stack (extractConstructor trueRef.Value) x -> true
    | x when satisfiesConstructor stack (extractConstructor falseRef.Value) x -> false
    | x ->
        let (Type(otn, _)) = getType stack evaluated
        let (Type(rtn, _)) = getType stack x
        raise (truthNotBoolException otn rtn)

and defaultOperator stack opName value =
    let (Type(name, ns)) = getType stack value
    let prefixOpName = "(" + opName + ")"

    if not (ns.ContainsKey prefixOpName) then
        raise (noMemberError name prefixOpName)
    else
        Ref(ns, topLevelGetter prefixOpName, topLevelSetter prefixOpName)

and optionalOperator op =
    Value.Function(fun st v ->
        if typeEq (getType st v) noneTypeRef.Value then
            Value.Function(fun _ _ -> none' ())
        else
            Value.Application(op, v))

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
        let (Object(_, args')) = dereference stack value
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

        let res =
            let (NamespacedName(_, name')) = nsName
            let ln = lastName name'
            Some(withSet (Dictionary()) ln (Val value))

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
            | _ -> res
        | _ -> res
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
        | head :: tail -> compileConstructor stack (extractConstructor nodeRef.Value) [ head; ListPattern tail ] value
    | HeadTailPattern(head, tail) -> compileConstructor stack (extractConstructor nodeRef.Value) [ head; tail ] value
    | _ -> failwith "Trying to compile not-a-pattern"

and evalAll (stack: ContextStack) =
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

and performEquation stack (closureCtx: Context) (Equation(lhs, rhs)) =
    let stack' = closureCtx :: stack

    let patternResult =
        compilePattern stack' lhs (Closure(Unrecognizable rhs, [ closureCtx ], false))

    match patternResult with
    | None -> raise patternFailed
    | Some ctx ->
        match combine2 closureCtx ctx with
        | Ok(ctx') -> ctx'
        | Error key -> raise (incorrectNameBinding key)

and performEval stack closureCtx (Eval expr) =
    let stack' = closureCtx :: stack
    evalAll stack' (Closure(Unrecognizable expr, [ closureCtx ], false)) |> ignore
    closureCtx

and performType
    stack
    (closureCtx: Context)
    (TypeDeclaration(NamespacedName(_, Identifier(typeName)), constructors, body))
    =
    let members: Context = Dictionary()

    if closureCtx.ContainsKey(typeName) then
        raise (incorrectNameBinding typeName)

    let type' = Type(typeName, members)

    let closureClxWithConstructors =
        FSharp.Collections.List.fold
            (fun (ctx: Context) (ConstructorDeclaration(NamespacedName(_, Identifier(cname)), args)) ->
                if ctx.ContainsKey(cname) then
                    raise (incorrectNameBinding cname)

                let constructor = Constructor(cname, FSharp.Collections.List.length args, type')
                withSet ctx cname (Val constructor))
            closureCtx
            constructors

    let finalClosureCtx =
        (withSet closureClxWithConstructors typeName (Namespace members))

    let stack' = members :: finalClosureCtx :: stack
    let members' = FSharp.Collections.List.fold (performEquation stack') members body

    let internalFuncs = [ "inst" ]

    FSharp.Collections.List.fold
        (fun _ x ->
            if members'.ContainsKey(x) then
                raise (notOverridable x)
            else
                ())
        ()
        internalFuncs

    let members'' =
        members'
        |> applyDefaultTruth
        |> applyDefaultEquality
        |> applyDefaultInequality
        |> applyDefaultNegation
        |> applyDefaultNegation

    withSet finalClosureCtx typeName (Namespace members'')

and applyDefaultTruth (members: Context) =
    let defaultTruth =
        Value.Function(fun st v ->
            let (Object(Constructor(_, argsCount, _), _)) = dereference st v
            if argsCount = 0 then false' () else true' ())

    if members.ContainsKey "truth" then
        members
    else
        withSet members "truth" (Val defaultTruth)

and applyDefaultEquality (members: Context) =
    let defaultEquality =
        Value.Function(fun _ _ -> Value.Function(fun _ _ -> false' ()))

    if members.ContainsKey "(=)" then
        members
    else
        withSet members "(=)" (Val defaultEquality)

and applyDefaultInequality (members: Context) =
    let defaultInequality =
        let eqOp = Unrecognizable(NamespacedName([], Operator("=", false)))
        let negOp = Unrecognizable(NamespacedName([], Operator("!", false)))

        Value.Function(fun _ v ->
            Value.Function(fun _ v' -> Value.Application(negOp, Value.Application(Value.Application(eqOp, v), v'))))

    if members.ContainsKey "(!=)" then
        members
    else
        withSet members "(!=)" (Val defaultInequality)

and applyDefaultNegation (members: Context) =
    let defaultNegation =
        Value.Function(fun st v -> if checkCondition st v then false' () else true' ())

    if members.ContainsKey "(!)" then
        members
    else
        withSet members "(!)" (Val defaultNegation)

and applyDefaultRepr (members: Context) =
    let rec repr st v =
        let (Object(Constructor(name, _, _), args)) = dereference st v

        match args with
        | Empty -> name
        | Has _ -> name + "(" + String.concat ", " (Seq.map (repr st) args) + ")"

    let defaultRepr = Value.Function(fun st v -> String(repr st v))

    if members.ContainsKey "repr" then
        members
    else
        withSet members "repr" (Val defaultRepr)

let rec dereferenceAll stack v =
    let evaluated = dereference stack (evalAll stack v)

    match evaluated with
    | Object(cons, args) -> Object(cons, map (dereferenceAll stack) args)
    | _ -> evaluated
