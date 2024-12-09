module Interpreter.Builtins.FLoat

open System.Collections.Generic
open Interpreter.Builtins.BuiltinRefs
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Value

let floatMembers: Context = Dictionary() |> applyDefaultNegation |> applyDefaultInequality
let floatType = Type("Float", floatMembers)

let isFloat stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Float _ -> true' ()
    | _ -> false' ()

floatMembers.Add("inst", Val(Function(isFloat)))

let floatTruth stack v =
    let evaluated = dereference stack v

    match evaluated with
    | Float(0.) -> false' ()
    | Float _ -> true' ()
    | _ -> raise (unexpectedType "float.truth")

floatMembers.Add("truth", Val(Function(floatTruth)))

let floatRepr =
    Function(fun st v ->
        let ev = dereference st v

        match ev with
        | Float x -> String(x |> string)
        | _ -> raise (unexpectedType "Float.repr"))

floatMembers.Add("repr", Val floatRepr)

let floatPow =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> Float(pown x y)
                | Float y -> Float(x ** y)
                | _ -> raise (unexpectedType "Float.(**)")
            | _ -> raise (unexpectedType "Float.(**)")))

floatMembers.Add("(**)", Val floatPow)

let floatMul =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> Float(x * float y)
                | Float y -> Float(x * y)
                | _ -> raise (unexpectedType "Float.(*)")
            | _ -> raise (unexpectedType "Float.(*)")))

floatMembers.Add("(*)", Val floatMul)

let floatTrueDiv =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> Float(x / float y)
                | Float y -> Float(x / y)
                | _ -> raise (unexpectedType "Float.(/)")
            | _ -> raise (unexpectedType "Float.(/)")))
floatMembers.Add("(/)", Val floatTrueDiv)

let floatDiv =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> Int(x / float y |> floor |> int)
                | Float y -> Int(x / y |> floor |> int)
                | _ -> raise (unexpectedType "Float.(//)")
            | _ -> raise (unexpectedType "Float.(//)")))
floatMembers.Add("(//)", Val floatDiv)

let floatMod =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y ->
                    let y' = float y
                    Float(x - y' * (x / y' |> floor))
                | Float y -> Float(x - y * (x / y |> floor))
                | _ -> raise (unexpectedType "Float.(%)")
            | _ -> raise (unexpectedType "Float.(%)")))
floatMembers.Add("(%)", Val floatMod)

let floatAdd =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> Float(x + float y)
                | Float y -> Float(x + y)
                | _ -> raise (unexpectedType "Float.(+)")
            | _ -> raise (unexpectedType "Float.(+)")))
floatMembers.Add("(+)", Val floatAdd)

let floatSub =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> Float(x - float y)
                | Float y -> Float(x - y)
                | _ -> raise (unexpectedType "Float.(-)")
            | _ -> raise (unexpectedType "Float.(-)")))
floatMembers.Add("(-)", Val floatSub)

let floatLt =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> if x < float y then true'() else false'()
                | Float y -> if x <  y then true'() else false'()
                | _ -> raise (unexpectedType "Float.(<)")
            | _ -> raise (unexpectedType "Float.(<)")))
floatMembers.Add("(<)", Val floatLt)

let floatGt =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> if x > float y then true'() else false'()
                | Float y -> if x >  y then true'() else false'()
                | _ -> raise (unexpectedType "Float.(>)")
            | _ -> raise (unexpectedType "Float.(>)")))
floatMembers.Add("(>)", Val floatGt)

let floatLe =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> if x <= float y then true'() else false'()
                | Float y -> if x <=  y then true'() else false'()
                | _ -> raise (unexpectedType "Float.(<=)")
            | _ -> raise (unexpectedType "Float.(<=)")))
floatMembers.Add("(<=)", Val floatLe)

let floatGe =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> if x >= float y then true'() else false'()
                | Float y -> if x >=  y then true'() else false'()
                | _ -> raise (unexpectedType "Float.(>=)")
            | _ -> raise (unexpectedType "Float.(>=)")))
floatMembers.Add("(>=)", Val floatGe)

let floatEq =
    Function(fun st v ->
        Function(fun st' v' ->
            let ev = dereference st v
            let ev' = dereference st' v'

            match ev with
            | Float x ->
                match ev' with
                | Int y -> if x = float y then true'() else false'()
                | Float y -> if x = y then true'() else false'()
                | _ -> false'()
            | _ -> raise (unexpectedType "Float.(=)")))
floatMembers.Add("(=)", Val floatEq)
