﻿open System.Collections.Generic
open FParsec
open Parser
open System
open Parser.Main

[<EntryPoint>]
let main argv =
    let parser = program
    let toParse = "let x = match a {case b.c -> None }"
    
    match run parser toParse with
    | Success(node, _, _) -> printf $"S:{node}"
    | Failure(s, _, _) -> printf $"F:{s}"

    0 // return an integer exit code
