module Interpreter.Exceptions

exception NameError of string
let nameError s = NameError $"Unknown constant %s{s}"

exception ConstructorError of string
let constructorError cn tn ex gn =
    ConstructorError $"Constructor %s{cn} of type %s{tn} has %d{ex} arguments, but was given %d{gn}"

exception PatternError of string
let patternFailed = PatternError "Patter matching failed"
let incorrectNameBinding name = PatternError $"Name %s{name} was bind twice"

exception TypeError of string
let notCallableError = TypeError "Non-callable object called"
