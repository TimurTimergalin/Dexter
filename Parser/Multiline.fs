module Parser.Multiline

open FParsec
open Parser.Spaces
open Parser.debug

let multiline1 p allowEof =
    let sep' = if allowEof then eof <|> sepSymbol else sepSymbol
    (pipe2 p (many (ws >>? sep' >>? anyWs >>. p)) <| fun first rest -> first::rest) .>> opt (ws >>? skipChar ';') 
    
let multiline p allowEof=
    multiline1 p allowEof <|>% []
    

