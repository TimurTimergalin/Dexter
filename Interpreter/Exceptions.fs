module Interpreter.Exceptions

exception NameError of string
let nameError s = NameError $"Unknown constant %s{s}"

exception ConstructorError of string

let constructorError cn tn ex gn =
    ConstructorError $"Constructor %s{cn} of type %s{tn} has %d{ex} arguments, but was given %d{gn}"
