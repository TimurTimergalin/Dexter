module Parser.Lists

open FParsec
open Parser.Spaces
open Parser.debug

let commaSeparated p forceInline trail =
    let sp = if forceInline then ws else anyWs
    let res = pipe2 p <| many (attempt (sp >>. skipChar ',' >>. sp >>? p)) <| fun f rest -> f::rest
    if trail then res .>> opt (attempt (sp >>. skipChar ',')) else res

let listOf p =
    skipChar '[' >>. anyWs >>. ((skipChar ']' >>% []) <|> (commaSeparated p false true .>> anyWs .>> skipChar ']'))
