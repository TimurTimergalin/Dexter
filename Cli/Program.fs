open System
open Interpreter.Exceptions
open Interpreter.Main

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printf $"Args: filename.dxt"
        1
    else
        try
            runDexter argv[0] "Std" "Lib"
        with
        | :? DexterError as e ->
            printf $"%s{e.Data0}: %s{e.Data1}"
        | :? Exception as e ->
            printf $"System error: %s{e.Message}"
        0
