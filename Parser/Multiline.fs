module Parser.Multiline

open FParsec
open Parser.Spaces

let multiline1 p allowEof =
    let sep' = if allowEof then eof <|> sepSymbol else sepSymbol

    (pipe2 p (many (skipMany1 (ws >>? sep' >>? anyWs >>? notFollowedByEof) >>? p)) <| fun first rest -> first :: rest)
    .>> skipMany (anyWs >>? skipChar ';')

let multiline p allowEof = multiline1 p allowEof <|>% []
