module Parser.Names
open System
open FParsec
open Parser.Ast
open Parser.Context
open Parser.Spaces

let allowedNameChar c = System.Char.IsLetter c || System.Char.IsDigit c || c = '_' || c = '''

let name: DexterParser<_> =
    many1Satisfy2L (fun x -> allowedNameChar x && not (System.Char.IsDigit x)) allowedNameChar "valid identifier name"

let reservedKeywordsList = [
    "if"
    "then"
    "else"
    "match"
    "case"
    "fun"
    "_"
    "let"
    "type"
    "exec"
    "entrypoint"
    "do"
    "eval"
    "import"
    "from"
]

let keyword name = attempt <| skipString name

let reservedKeyword: DexterParser<_> =
    reservedKeywordsList |> List.map keyword |> choice .>> notFollowedBy (skipSatisfy allowedNameChar)

let allowedName = notFollowedByL reservedKeyword "keyword" >>. name

let identifier = allowedName |>> Identifier

let plainName = identifier |>> fun id -> NamespacedName([], id)

let allowedOperatorChar: DexterParser<_> = anyOf "!@$%=^:&*+-|/\\~><"

let illegalOperatorList = [
    "."
    ","
    "->"
    "<-"
]

let unaryOperatorList = [
    "!"
    "~"
    "^+"
    "^-"
]

let illegalOperator lst =
    lst |> List.map skipString |> List.map attempt |> choice .>> notFollowedBy allowedOperatorChar

let illegalInfixOperator = illegalOperator (List.concat [illegalOperatorList;unaryOperatorList])
let illegalOperatorDef = illegalOperator illegalOperatorList

let anyOperator = many1 allowedOperatorChar |>> (List.map string >> String.concat "")

let operator' illegal = notFollowedByL illegal "illegal operator" >>. anyOperator

let unaryOperator' = pstring "!" <|> pstring "!" <|> stringReturn "+" "^+" <|> stringReturn "-" "^-" .>> notFollowedBy allowedOperatorChar

let optionalOperator pop = pipe2 pop (opt <| skipChar '?') <|
                           fun op isOpt -> match isOpt with
                                           | None -> Operator(op, false)
                                           | Some(_) -> Operator(op, true)

let operator : DexterParser<_> = optionalOperator <| operator' illegalInfixOperator

let operatorDef' pop =
    skipChar '(' >>. anyWs >>. pop .>> anyWs .>> skipChar ')'

let prefixOperator: DexterParser<_> = operatorDef' (optionalOperator (operator' illegalOperatorDef))
let operatorDef = operatorDef' ((operator' illegalOperatorDef |>> fun x -> Operator(x, false)) .>> notFollowedByL (skipChar '?') "optional operator")

let unaryOperator = optionalOperator unaryOperator'
    
let namespacedName, namsepacedNameRef = createParserForwardedToRef<Node, ParserContext>()
namsepacedNameRef.Value <-
    let mergeResults token (NamespacedName(lst, id)) =
        NamespacedName(token::lst, id)
    
    attempt ((identifier <|> prefixOperator) .>> notFollowedBy (skipChar '.') |>> (fun x -> NamespacedName([], x))) <|>
    (pipe2 (name .>> skipChar '.' .>> anyWs) namespacedName mergeResults)
