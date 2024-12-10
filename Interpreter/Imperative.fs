module Interpreter.Imperative

open System
open System.Collections.Generic

// Приведенные ниже функции позволяют работать с изменяемыми Dictionary и List в той же манере, что и с
// неизменяемыми Map и list (т.е. в функциональном стиле).

// Весь императивный код интерпретатора представлен в этом модуле и обернут в безопасные функциональные обертки
// Также в этом модуле представлены другие полезные методы для работы с изменяемыми типами данных


let withSet (dict: Dictionary<'a, 'b>) key value =
    dict[key] <- value
    dict

let withSetSafe (dict: Dictionary<'a, 'b>) key value =
    try
        dict.Add(key, value)
        Ok(dict)
    with :? ArgumentException ->
        Error ()

let combine2 (dict1: Dictionary<'a, 'b>) (dict2: Dictionary<'a, 'b>) =
    let mutable result = Ok(dict1)

    for key in dict2.Keys do
        if dict1.ContainsKey key then
            result <- Error(key)
        else
            dict1[key] <- dict2[key]

    result

let combine (dicts: Dictionary<'a, 'b> seq) (saveTo: Dictionary<'a, 'b>) =
    dicts
    |> Seq.fold
        (fun st new' ->
            match st with
            | Error _ as e -> e
            | Ok v -> combine2 v new')
        (Ok(saveTo))


let withAdded (lst: List<'a>) v =
    lst.Add v
    lst

let withPopped (lst: List<'a>) =
    lst.RemoveAt(lst.Count - 1)
    lst

let withUpdated (lst: List<'a>) at value =
    lst[at] <- value
    lst

let map f lst =
    Seq.fold (fun st i -> withUpdated st i (f (st[i]))) lst (seq { 0 .. (lst.Count - 1) })

let mapi f lst =
    Seq.fold (fun st i -> withUpdated st i (f i (st[i]))) lst (seq { 0 .. (lst.Count - 1) })

let head (lst: List<'a>) = lst.Item(lst.Count - 1)

let tail (lst: List<'a>) = lst.GetRange(0, lst.Count - 1)

let (|Empty|Has|) (lst: List<'a>) =
    if lst.Count = 0 then Empty else Has(head lst, tail lst)

let reversed (lst: List<'a>) =
    let rec inner acc =
        function
        | Empty -> acc
        | Has(head, tail) -> inner (withAdded acc head) tail

    inner (List()) lst
