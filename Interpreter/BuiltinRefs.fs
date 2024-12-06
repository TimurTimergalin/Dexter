module Interpreter.BuiltinRefs

open Interpreter.Value

// Все ниже проделанные махинации - не императивщина.
// Все это нужно лишь для того, чтобы позволить модулям Builtins и Main ссылаться друг на друга.
// Это необходимо, так как файлы в F# компилируются строго в определенном порядке.
// Можно сказать, что BuiltinRefs - "заголовочный файл" для модуля Builtins.
// Отметим, что объединение в один файл все равно бы не помогло - в таком случае эти "Заголовки" пришлось бы вставить и туда 

let noneRef = ref (Action <| fun () -> failwith "not init")
let noneTypeRef = ref (Action <| fun () -> failwith "not init")

let trueRef = ref (Action <| fun () -> failwith "not init")
let falseRef = ref (Action <| fun () -> failwith "not init")
let boolRef = ref (Action <| fun () -> failwith "not init")

let nodeRef = ref (Action <| fun () -> failwith "not init")
let endRef = ref (Action <| fun () -> failwith "not init")
let listRef = ref (Action <| fun () -> failwith "not init")

let true'() = Object(trueRef.Value, [])
let false'() = Object(falseRef.Value, [])
let none'() = Object(noneRef.Value, [])
