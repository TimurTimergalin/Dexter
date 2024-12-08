module Interpreter.Exceptions

exception NameError of string
let nameError s = NameError $"Unknown constant '%s{s}'"

exception ConstructorError of string

let constructorError cn tn ex gn =
    ConstructorError $"Constructor '%s{cn}' of type '%s{tn}' has %d{ex} arguments, but was given %d{gn}"

exception PatternError of string
let patternFailed = PatternError "Patter matching failed"

let incorrectNameBinding name =
    PatternError $"Name '%s{name}' was bind twice"

exception TypeError of string
let notCallableError = TypeError "Non-callable object called"

let noMemberError tn mn =
    TypeError $"Type '%s{tn}' does not have a member '%s{mn}'"

let truthNotBoolException objTn resTn =
    TypeError $"When resolving condition, 'truth' of type '%s{objTn}' returned type '%s{resTn}' instead of bool"
