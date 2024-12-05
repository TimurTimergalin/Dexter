module Interpreter.Imperative

open System.Collections.Generic

// Приведенные ниже функции позволяют работать с изменяемыми Dictionary и List в той же манере, что и с
// неизменяемыми Map и list (т.е. в функциональном стиле)

let withSet (dict: Dictionary<'a, 'b>) key value =
    dict.Remove(key) |> ignore
    dict.Add(key, value)
    dict

let withRemoved (dict: Dictionary<'a, 'b>) key =
    if not (dict.Remove(key)) then
        failwith "Tried to remove non-existing element from dictionary"
    dict

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