module Interpreter.Builtins.Int

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Exceptions
open Interpreter.Evaluate
open Interpreter.Value

let intMembers: Context = Dictionary() |> applyDefaultNegation |> applyDefaultInequality
let intType = Type("Int", intMembers)

let isInt stack v =
    let toApply v =
        let evaluated = dereference stack v

        match evaluated with
        | Int _ -> true' ()
        | _ -> false' ()
    ApplyAfter(v, stack, defaultForceStop, [toApply], [], [])

intMembers.Add("inst", Val(Function(isInt)))

let intTruth stack v =
    let toApply v =
        let evaluated = dereference stack v

        match evaluated with
        | Int(0) -> false' ()
        | Int _ -> true' ()
        | _ -> raise (unexpectedType "Int.truth")
    ApplyAfter(v, stack, defaultForceStop, [toApply], [], [])

intMembers.Add("truth", Val(Function(intTruth)))

let intRepr =
    Function(
        fun st v ->
            let toApply v =
                let ev = dereference st v
                match ev with
                | Int x -> String (x |> string)
                | _ -> raise (unexpectedType "Int.repr")
            ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("repr", Val intRepr)

let intAdd =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'

                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x + y)
                        | Float y -> Float((float x) + y)
                        | _ -> raise (unexpectedType "Int.(+)")
                    | _ -> raise (unexpectedType "Int.(+)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(+)", Val intAdd)

let intPow =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(pown x y)
                        | Float y -> Float((float x)**y)
                        | _ -> raise (unexpectedType "Int.(**)")
                    | _ -> raise (unexpectedType "Int.(**)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(**)", Val intPow)

let intMul =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x * y)
                        | Float y -> Float((float x) * y)
                        | _ -> raise (unexpectedType "Int.(*)")
                    | _ -> raise (unexpectedType "Int.(*)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(*)", Val intMul)

let intTrueDiv =
    Function(fun st v ->
        let toApply v =  
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Float((float x) / (float y))
                        | Float y -> Float((float x) / y)
                        | _ -> raise (unexpectedType "Int.(*)")
                    | _ -> raise (unexpectedType "Int.(*)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(/)", Val intTrueDiv)

let intDiv =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x / y)
                        | Float y -> Int((float x) / y |> floor |> int)
                        | _ -> raise (unexpectedType "Int.(//)")
                    | _ -> raise (unexpectedType "Int.(//)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(//)", Val intDiv)

let intMod =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x % y)
                        | Float y -> Float((float x) - y * ((float x) / y |> floor))
                        | _ -> raise (unexpectedType "Int.(%)")
                    | _ -> raise (unexpectedType "Int.(%)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(%)", Val intMod)

let intSub =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'

                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x - y)
                        | Float y -> Float((float x) - y)
                        | _ -> raise (unexpectedType "Int.(-)")
                    | _ -> raise (unexpectedType "Int.(-)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(-)", Val intSub)

let intRShift =
    Function(fun st v ->
        let toApply v = 
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'

                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x >>> y)
                        | _ -> raise (unexpectedType "Int.(>>>)")
                    | _ -> raise (unexpectedType "Int.(>>>)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(>>>)", Val intRShift)

let intLShift =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'

                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x <<< y)
                        | _ -> raise (unexpectedType "Int.(<<<)")
                    | _ -> raise (unexpectedType "Int.(<<<)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(<<<)", Val intLShift)

let intLt =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> if x < y then true'() else false'()
                        | Float y -> if float x < y then true'() else false'()
                        | _ -> raise (unexpectedType "Int.(<)")
                    | _ -> raise (unexpectedType "Int.(<)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(<)", Val intLt)

let intGt =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> if x > y then true'() else false'()
                        | Float y -> if float x > y then true'() else false'()
                        | _ -> raise (unexpectedType "Int.(>)")
                    | _ -> raise (unexpectedType "Int.(>)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(>)", Val intGt)

let intLe =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> if x <= y then true'() else false'()
                        | Float y -> if float x <= y then true'() else false'()
                        | _ -> raise (unexpectedType "Int.(<=)")
                    | _ -> raise (unexpectedType "Int.(<=)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(<=)", Val intLe)

let intGe =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> if x >= y then true'() else false'()
                        | Float y -> if float x >= y then true'() else false'()
                        | _ -> raise (unexpectedType "Int.(>=)")
                    | _ -> raise (unexpectedType "Int.(>=)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(>=)", Val intGe)

let intEq =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'
                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> if x = y then true'() else false'()
                        | Float y -> if float x = y then true'() else false'()
                        | _ -> true'()
                    | _ -> raise (unexpectedType "Int.(=)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(=)", Val intEq)

let intBitAnd =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Int x ->
                match ev' with
                | Int y -> Int(x &&& y)
                | _ -> raise (unexpectedType "Int.(&)")
            | _ -> raise (unexpectedType "Int.(&)")))
intMembers.Add("(&)", Val intBitAnd)

let intBitOr =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'

                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x ||| y)
                        | _ -> raise (unexpectedType "Int.(|)")
                    | _ -> raise (unexpectedType "Int.(|)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(|)", Val intBitOr)

let intBitXor =
    Function(fun st v ->
        let toApply v =
            Function(fun st' v' ->
                let toApply v' =
                    let ev = dereference st v
                    let ev' = dereference st' v'

                    match ev with
                    | Int x ->
                        match ev' with
                        | Int y -> Int(x ^^^ y)
                        | _ -> raise (unexpectedType "Int.(^")
                    | _ -> raise (unexpectedType "Int.(^)")
                ApplyAfter(v', st', defaultForceStop, [toApply], [], [])
            )
        ApplyAfter(v, st, defaultForceStop, [toApply], [], [])
    )
intMembers.Add("(^)", Val intBitXor)


