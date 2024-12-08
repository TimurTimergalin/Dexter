module Parser.Literals

open FParsec
open Parser.Ast
open Parser.Context

let stringLiteral: DexterParser<_> =
    let stringContent (closingQuote: char) =
        let escapedCharacter (closingQuote: char) =
            skipChar '\\' >>. (anyOf "\\abfnrtv" <|> pchar closingQuote)
            |>> function
                | '\\' -> '\\'
                | 'a' -> '\a'
                | 'b' -> '\b'
                | 'f' -> '\f'
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | 'v' -> '\v'
                | x when x = closingQuote -> closingQuote
                | x -> failwith "escapedCharacter encountered unxepected symbol: " + x

        let normalCharacter (closingQuote: char) =
            satisfy (fun x -> x <> '\\' && x <> closingQuote)

        many (normalCharacter closingQuote <|> escapedCharacter closingQuote)
        .>> skipChar closingQuote
        |>> (List.map string >> String.concat "")

    (skipChar '\'' >>. stringContent '\'') <|> (skipChar '"' >>. stringContent '"')
    |>> StringLiteral

let numberFormat =
    NumberLiteralOptions.AllowBinary
    ||| NumberLiteralOptions.AllowExponent
    ||| NumberLiteralOptions.AllowFraction
    ||| NumberLiteralOptions.AllowHexadecimal
    ||| NumberLiteralOptions.AllowOctal
    ||| NumberLiteralOptions.AllowMinusSign
    ||| NumberLiteralOptions.AllowPlusSign

let numericalLiteral: DexterParser<_> =
    (numberLiteral numberFormat "number"
     |>> fun x ->
         if x.IsInteger then
             IntLiteral(int x.String)
         else
             FloatLiteral(float x.String))
    .>> notFollowedByL (satisfy System.Char.IsLetter) "letter"
