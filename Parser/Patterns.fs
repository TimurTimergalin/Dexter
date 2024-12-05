module Parser.Patterns

open FParsec
open Parser.Ast
open Parser.Context
open Parser.Lists
open Parser.Literals
open Parser.Names
open Parser.Spaces

let tier0Pattern, tier0PatternRef =
    createParserForwardedToRef<Node, ParserContext>()

let tier0ValuelessPattern, tier0ValuelessPatternRef =
    createParserForwardedToRef<Node, ParserContext>()
    
let pattern valueless = if valueless then tier0ValuelessPattern else tier0Pattern

let patternInParenthesis valueless =
    skipChar '(' >>. anyWs >>. pattern valueless .>> anyWs .>> skipChar ')'

let skipPattern: DexterParser<_> = skipChar '_' >>% SkipPattern

let literalPattern =
    choice [ stringLiteral |>> LiteralPattern; numericalLiteral |>> LiteralPattern ]

let listPattern valueless = listOf (pattern valueless) |>> ListPattern

let nameBind =
    plainName
    .>> notFollowedByL (skipChar '.' <|> (ws >>? skipChar '(')) "namespaced identifier or constructor call"

let constructorPattern valueless =
    pipe2 namespacedName
    <| (ws >>. skipChar '(' >>. anyWs >>. commaSeparated (pattern valueless) false false
        .>> anyWs
        .>> skipChar ')')
    <| fun name args -> ConstructorPattern(name, args)

let tier1Pattern valueless =
    let res = choice [
          skipPattern
          attempt nameBind
          constructorPattern valueless
          listPattern valueless
          patternInParenthesis valueless
          ]
    if valueless then res else res <|> literalPattern

let tier0patternComb valueless =
    pipe2 (tier1Pattern valueless)
    <| opt (attempt (ws >>? skipString "::" >>? anyWs >>? (pattern valueless)))
    <| fun first rest ->
        match rest with
        | None -> first
        | Some pat -> HeadTailPattern(first, pat)

tier0PatternRef.Value <- tier0patternComb false
tier0ValuelessPatternRef.Value <- tier0patternComb true
    
