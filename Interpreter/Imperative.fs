module Interpreter.Imperative

open System.Collections.Generic

// Приведенные ниже функции позволяют работать с изменяемыми Dictionary и List в той же манере, что и с
// неизменяемыми Map и list (т.е. в функциональном стиле).

// Весь императивный код интерпретатора представлен в этом модуле и обернут в безопасные функциональные обертки
// Также в этом модуле представлены другие полезные методы для работы с изменяемыми типами данных

// Данный метод изменяет состояние словаря dict.
// Во избежание нежелательных побочных эффектов, значения в словаре нужно менять только на логически эквивалентные
// (например, невычисленное значение на вычисленное)
let withSet (dict: Dictionary<'a, 'b>) key value =
    dict[key] <- value
    dict
    
let combine2 (dict1: Dictionary<'a, 'b>) (dict2: Dictionary<'a, 'b>) =
    let mutable result = Ok(dict1)
    for key in dict2.Keys do
        if dict1.ContainsKey key then
            result <- Error("key")
        else
            dict1[key] <- dict2[key]
    result
    
let combine (dicts: Dictionary<'a, 'b> seq) =
    dicts |> Seq.fold (fun st new' -> match st with Error _ as e -> e | Ok v -> combine2 v new') (Ok(Dictionary()))
