module Interpreter.Exceptions

exception DexterError of string * string

let NameError x = DexterError("Name error", x) 
let unknownNameError s = NameError $"Unknown constant '%s{s}'"

let incorrectNameBinding name =
    NameError $"Name '%s{name}' was bind twice"

let ConstructorError x = DexterError("Constructor error", x)

let constructorError cn tn ex gn =
    ConstructorError $"Constructor '%s{cn}' of type '%s{tn}' has %d{ex} arguments, but was given %d{gn}"

let PatternError x = DexterError("Pattern Error", x)
let patternFailed = PatternError "Patter matching failed"

let TypeError x = DexterError("Type error", x)
let notCallableError = TypeError "Non-callable object called"
let noMemberError tn mn =
    TypeError $"Type '%s{tn}' does not have a member '%s{mn}'"
let truthNotBoolException objTn resTn =
    TypeError $"When resolving condition, 'truth' of type '%s{objTn}' returned type '%s{resTn}' instead of bool"
let notOverridable name =
    TypeError $"Internal member function %s{name} cannot be overridden"

let EntrypointError x = DexterError("Entrypoint error", x)
let redefinitionError file =
    EntrypointError $"2 entry points encountered in %s{file}"

let SyntaxError x = DexterError("Syntax error", x)

let ImportError x = DexterError("Import error", x)
let circularImport name =
    ImportError $"File '%s{name}' is being import while not fully initialized"
let invalidExtension name =
    ImportError $"File '%s{name}' does not have a proper extension '.dxt', so it cannot be imported"
let sourceNotFound name =
    ImportError $"File '%s{name}' does not exist"
