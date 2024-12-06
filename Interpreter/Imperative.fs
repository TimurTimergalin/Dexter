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

let withAdded (lst: List<'a>) v =
    lst.Add v
    lst

let withPopped (lst: List<'a>) =
    lst.RemoveAt(lst.Count - 1)
    lst

let head (lst: List<'a>) =
    lst.Item(lst.Count - 1)

let tail (lst: List<'a>) =
    lst.GetRange(0, lst.Count - 1)

let (|Empty|Has|) (lst: List<'a>) =
    if lst.Count = 0 then
        Empty
    else
        Has(head lst, tail lst)
 
 
let rec extendStack (acc: List<'a>) = function
        | Empty -> ()
        | Has(head, tail) ->
            acc.Add(head)
            extendStack acc tail

let reversed stack =
    let res = List()
    extendStack res stack
    res

let concatenated stack1 stack2 =
    extendStack stack1 (reversed stack2)
    stack1

let withExtended (stack1: List<'a>) (stack2: List<'a>) f =
    extendStack stack1 stack2
    let result = f stack1
    for i in 1..stack2.Count do
        stack1.RemoveAt(stack1.Count - 1)
    
    result
