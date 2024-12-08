module Parser.Spaces

open FParsec
open Parser.Context

let newLine: DexterParser<_> =
    (opt (skipString "#" >>. skipRestOfLine false) >>. newline) |>> ignore

let sepSymbol: DexterParser<_> = skipChar ';' <|> newLine

let ws: DexterParser<_> = skipMany (anyOf " \t")

let ws1: DexterParser<_> = skipMany1 (anyOf " \t")

let anyWs: DexterParser<_> = skipMany (ws1 <|> newLine)

let anyWs1: DexterParser<_> = skipMany1 (ws1 <|> newLine)
