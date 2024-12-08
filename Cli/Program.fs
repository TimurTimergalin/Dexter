open System
open Interpreter.Main

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printf $"Args: filename.dxt"
        1
    else
        try
            runDexter argv[0] "Std" "Lib"
        with :? Exception as e ->
            printf $"Error: %s{e.Message}"

        0
