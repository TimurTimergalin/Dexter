module Interpreter.Evaluate

open System.Collections.Generic
open System.IO
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

let patternRequiresEvaluation (Case(pattern, cond, _)) =
    if Option.isSome cond then
        true
    else
        match pattern with
        | SkipPattern
        | NameBind _ -> false
        | _ -> true

// Выражение будем называть узнаваемым, если у него можно определить тип, не применяя редукций
let rec recognizable =
    function
    | Unrecognizable _ -> false
    | Closure _ -> false
    | Ref(ctx, getter, _) ->
        let inner = getter ctx

        match inner with
        | Ref _ -> false
        | _ -> recognizable inner
    | Action _ -> false
    | Value.Application _ -> false
    | FlatApplication _ -> false
    | ApplyAfter _ -> false
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
            | Identifier n' ->
                let res = getRef stack nsName

                if n' = "acc" then
                    printf ""

                res
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
                let clr = pattern v (Dictionary())

                match clr with
                | None -> raise patternFailed
                | Some ctx -> Closure(Unrecognizable body, [ ctx ], false)

            Value.Function res
        | Match(valueNode, cases) ->
            let tempCtx =
                withSet (Dictionary()) "temp" (Val(Closure(Unrecognizable valueNode, stack, false)))

            let value = Ref(tempCtx, topLevelGetter "temp", topLevelSetter "temp")

            let checkCase value (Case(pattern, cond, body)) =
                let matchResult = compilePattern stack pattern value (Dictionary())

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

            let rec chooseCase value =
                function
                | [] -> raise patternFailed
                | case :: rest ->
                    match checkCase value case with
                    | None -> chooseCase value rest
                    | Some res -> res

            if not (List.exists patternRequiresEvaluation cases) then
                chooseCase value cases // Подменить на ApplyAfter
            else
                let toApply x = chooseCase x cases
                ApplyAfter(value, stack, defaultForceStop, [ toApply ], [], [])
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
        | Closure _ as clr -> clr
        | Ref _ as ref' -> ref'
        | Object(cons, args) -> Object(cons, map (fun v -> Closure(v, stack', isolated)) args)
        | Value.Function(f) -> Value.Function(fun st v -> Closure(f st v, stack', isolated))
        | Value.Application(f, arg) -> Value.Application(Closure(f, stack', isolated), Closure(arg, stack', isolated))
        | FlatApplication(f, args) ->
            FlatApplication(Closure(f, stack', isolated), List.map (fun x -> Closure(x, stack', isolated)) args)
        | ApplyAfter _ -> value' // В теории, никогда не случится
        | x when not (recognizable x) ->
            let toApply x' = Closure(x', stack', isolated)

            let forceStop (v: Value) =
                v.IsClosure
                || v.IsRef
                || v.IsObject
                || v.IsFunction
                || v.IsApplication
                || v.IsFlatApplication
                || v.IsApplyAfter

            ApplyAfter(x, stack', forceStop, [ toApply ], [], [])
        | x -> x
    | Ref(ctx, getter, setter) ->
        let inner = getter ctx

        match inner with
        | Ref _ -> inner
        | _ ->
            let toApply inner' =
                let ctx' = setter ctx inner'
                Ref(ctx', getter, setter)

            ApplyAfter(inner, [], defaultForceStop, [ toApply ], [], [])
    | Value.Application(f, arg) -> FlatApplication(f, [ arg ])
    | FlatApplication(f, args) ->
        match f with
        | FlatApplication(f', args') -> FlatApplication(f', List.concat [ args'; args ])
        | Value.Application(f', arg) -> FlatApplication(f', arg :: args)
        | x when not (recognizable x) ->
            let toApply x' = FlatApplication(x', args)
            ApplyAfter(x, stack, defaultForceStop, [ toApply ], [], [])
        | _ ->
            let evf = dereference stack f

            match evf with
            | Value.Function f ->
                match args with
                | [] -> failwith "Empty flat application"
                | [ arg ] -> f stack arg
                | arg :: rest -> FlatApplication(f stack arg, rest)
            | _ -> raise notCallableError
    | Action f -> f ()
    | ApplyAfter(v, stack', forceStop, fs, stackOfStacks, stackOfConditions) ->
        match v with
        | ApplyAfter(v', stack'', forceStop', fs', stackOfStacks', stackOfConditions') ->
            ApplyAfter(
                v',
                stack'',
                forceStop',
                List.concat [ fs'; fs ],
                List.concat [ stackOfStacks'; stack' :: stackOfStacks ],
                List.concat [ stackOfConditions'; forceStop :: stackOfConditions ]
            )
        | x when not (recognizable x) && not (forceStop x) -> ApplyAfter(eval1 stack' v, stack', forceStop, fs, stackOfStacks, stackOfConditions)
        | _ ->
            match fs with
            | [] -> failwith "Empty ApplyAfter"
            | [f] -> f v
            | f :: restF ->
                let nextV = f v
                let nextStack::restStacks = stackOfStacks
                let nextCond::restConds = stackOfConditions
                ApplyAfter(nextV, nextStack, nextCond, restF, restStacks, restConds)
    | x -> x

and evalUntilRecognizable (stack: ContextStack) (value: Value) =
    // printfn $"{value}\n-----\n"
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
        raise (truthNotBoolException otn)

and defaultOperator stack opName value =
    let toApply value =
        let (Type(name, ns)) = getType stack value
        let prefixOpName = "(" + opName + ")"

        if not (ns.ContainsKey prefixOpName) then
            raise (noMemberError name prefixOpName)
        else
            Ref(ns, topLevelGetter prefixOpName, topLevelSetter prefixOpName)
    ApplyAfter(value, stack, defaultForceStop, [toApply], [], [])

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
    (saveTo: Context)
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
            subtasks
            |> Seq.map (fun (n, v) -> compilePattern stack n v (Dictionary()))
            |> Seq.toList

        if not (List.forall Option.isSome results) then
            None
        else
            let someResults = results |> List.map Option.get
            let ctx' = combine someResults saveTo

            match ctx' with
            | Ok ctx'' -> Some ctx''
            | Error name -> raise (incorrectNameBinding name)

and compilePattern (stack: ContextStack) (node: Node) (value: Value) (saveTo: Context) : Context option =
    match node with
    | SkipPattern -> Some(saveTo)
    | NameBind nsName ->
        let ref' = getRefSafe stack nsName

        let res =
            let (NamespacedName(_, name')) = nsName
            let ln = lastName name'

            match withSetSafe saveTo ln (Val value) with
            | Error _ -> None
            | Ok(res') -> Some res'

        res
    | ConstructorPattern(nsName, args) ->
        let (Ref(ctx, getter, _)) = getRef stack nsName

        match getter ctx with
        | Constructor _ as cons -> compileConstructor stack cons args value saveTo
    | LiteralPattern literal ->
        let evaluated = dereference stack value

        match literal with
        | StringLiteral s ->
            match evaluated with
            | String s' when s' = s -> Some(saveTo)
            | _ -> None
        | IntLiteral i ->
            match evaluated with
            | Int i' when i = i' -> Some(saveTo)
            | Float f when float i = f -> Some(saveTo)
            | _ -> None
        | FloatLiteral f ->
            match evaluated with
            | Float f' when f = f' -> Some(saveTo)
            | Int i when f = float i -> Some(saveTo)
            | _ -> None
        | _ -> failwith "Unreachable: unknown literal"
    | ListPattern nodes ->
        match nodes with
        | [] ->
            let evaluated = dereference stack value

            match evaluated with
            | Object(Constructor("Empty", 0, type'), _) when typeEq type' listRef.Value -> Some(saveTo)
            | _ -> None
        | head :: tail ->
            compileConstructor stack (extractConstructor nodeRef.Value) [ head; ListPattern tail ] value saveTo
    | HeadTailPattern(head, tail) ->
        compileConstructor stack (extractConstructor nodeRef.Value) [ head; tail ] value saveTo
    | _ -> failwith "Trying to compile not-a-pattern"

and evalAll (stack: ContextStack) =
    function
    | x when not (recognizable x) -> evalAll stack (evalUntilRecognizable stack x)
    | Object(Constructor(name, argsCount, Type(tn, _)) as cons, args) ->
        let givenArgsCount = args.Count

        if givenArgsCount <> argsCount then
            raise (constructorError name tn argsCount givenArgsCount)

        let newArgs = map (evalAll stack) args
        Object(cons, newArgs)
    | Ref(ctx, getter, setter) ->
        let ctx' = setter ctx (evalAll stack (getter ctx))
        Ref(ctx', getter, setter)
    | x -> x

and performEquation stack (closureCtx: Context) (Equation(lhs, rhs)) =
    let stack' = closureCtx :: stack

    let patternResult =
        compilePattern stack' lhs (Closure(Unrecognizable rhs, stack', false)) closureCtx

    match patternResult with
    | None -> raise patternFailed
    | Some ctx -> ctx

and performEval stack closureCtx (Eval expr) =
    let stack' = closureCtx :: stack
    evalAll stack' (Closure(Unrecognizable expr, stack', false)) |> ignore
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

    let stack' = finalClosureCtx :: stack
    let members' = FSharp.Collections.List.fold (performEquation stack') members body

    let members'' =
        members'
        |> applyDefaultTruth
        |> applyDefaultEquality
        |> applyDefaultInequality
        |> applyDefaultNegation
        |> applyDefaultRepr
        |> applyDefaultInst

    withSet finalClosureCtx typeName (Namespace members'')

and applyDefaultTruth (members: Context) =
    let defaultTruth =
        Value.Function(fun st v ->
            let (Object(Constructor(_, argsCount, _), _)) = dereference st v
            if argsCount = 0 then false' () else true' ())

    let checkedTruth customTruth =
        Value.Function(fun st v ->
            let toEval = Value.Application(customTruth, v)
            let Type(tn, _) as type' = getType st toEval

            if not (typeEq type' boolRef.Value) then
                raise (truthNotBoolException tn)

            toEval)

    if members.ContainsKey "truth" then
        let (Val customTruth) = members["truth"]
        withSet members "truth" (Val(checkedTruth customTruth))
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

    let checkedRepr customRepr =
        Value.Function(fun st v ->
            let toEval = Value.Application(customRepr, v)
            let Type(tn, _) as type' = getType st toEval

            if not (typeEq type' stringRef.Value) then
                raise (reprNotStringException tn)

            toEval)

    if members.ContainsKey "repr" then
        let (Val customRepr) = members["repr"]
        withSet members "repr" (Val(checkedRepr customRepr))
    else
        withSet members "repr" (Val defaultRepr)

and applyDefaultInst (members: Context) =
    if members.ContainsKey "inst" then
        raise (notOverridable "inst")

    let inst =
        Value.Function(fun st v ->
            let (Type(_, ns)) = getType st v
            if ns = members then true' () else false' ())

    withSet members "inst" (Val inst)
