module Interpreter.Builtins.BuiltinRefs

open System.Collections.Generic
open Interpreter.Imperative
open Interpreter.Value

// Все махинации ниже нужны лишь для того, чтобы позволить модулям Builtins и Main ссылаться друг на друга.
// Это необходимо, так как файлы в F# компилируются строго в определенном порядке.
// Можно сказать, что BuiltinRefs - "заголовочный файл" для модуля Builtins.

let noneRef = ref (Action <| fun () -> failwith "not init")
let noneTypeRef = ref Absent

let trueRef = ref (Action <| fun () -> failwith "not init")
let falseRef = ref (Action <| fun () -> failwith "not init")
let boolRef = ref Absent

let nodeRef = ref (Action <| fun () -> failwith "not init")
let endRef = ref (Action <| fun () -> failwith "not init")
let listRef = ref Absent

let intRef = ref Absent
let floatRef = ref Absent
let stringRef = ref Absent
let functionRef = ref Absent

let reprRef = ref (Action <| fun () -> failwith "not init")
let printRef = ref (Action <| fun () -> failwith "not init")

let extractConstructor (Ref(ctx, getter, _)) =
    match getter ctx with
    | Constructor _ as cons -> cons
    | _ -> failwith "Not a constructor"

let true' () =
    Object(extractConstructor trueRef.Value, List())

let false' () =
    Object(extractConstructor falseRef.Value, List())

let none' () =
    Object(extractConstructor noneRef.Value, List())

let end' () =
    Object(extractConstructor endRef.Value, List())

let node' head tail =
    Object(extractConstructor nodeRef.Value, withAdded (withAdded (List()) head) tail)
