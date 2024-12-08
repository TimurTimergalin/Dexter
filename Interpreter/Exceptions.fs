module Interpreter.Exceptions

exception NameError of string
let unknownNameError s = NameError $"Unknown constant '%s{s}'"

let incorrectNameBinding name =
    NameError $"Name '%s{name}' was bind twice"

exception ConstructorError of string

let constructorError cn tn ex gn =
    ConstructorError $"Constructor '%s{cn}' of type '%s{tn}' has %d{ex} arguments, but was given %d{gn}"

exception PatternError of string
let patternFailed = PatternError "Patter matching failed"

exception TypeError of string
let notCallableError = TypeError "Non-callable object called"

let noMemberError tn mn =
    TypeError $"Type '%s{tn}' does not have a member '%s{mn}'"

let truthNotBoolException objTn resTn =
    TypeError $"When resolving condition, 'truth' of type '%s{objTn}' returned type '%s{resTn}' instead of bool"

let notOverridable name =
    TypeError $"Internal member function %s{name} cannot be overridden"

exception EntrypointError of string

let redefinitionError file =
    EntrypointError $"2 entry points encountered in %s{file}"

exception SyntaxError of string

exception ImportError of string

let circularImport name =
    ImportError $"File '%s{name}' is being import while not fully initialized"

let invalidExtension name =
    ImportError $"File '%s{name}' does not have a proper extension '.dxt', so it cannot be imported"

let sourceNotFound name =
    ImportError $"File '%s{name}' does not exist"
