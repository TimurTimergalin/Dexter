module Parser.Main

open System.Collections.Generic
open FParsec
open Parser.Ast
open Parser.Context
open Parser.Lists
open Parser.Literals
open Parser.Multiline
open Parser.Names
open Parser.Patterns
open Parser.Spaces
open Parser.debug

let tier0Expr, tier0ExprRef = createParserForwardedToRef<Node, ParserContext>()
let tier1Statement, tier1StatementRef = createParserForwardedToRef<Node, ParserContext>()

let exprInParentheses =
    skipChar '(' >>. anyWs >>. tier0Expr .>> anyWs .>> skipChar ')'

let listLiteral = listOf tier0Expr |>> ListLiteral

let compound =
    skipChar '{' >>. anyWs >>. multiline tier1Statement false .>> anyWs .>> skipChar '}' |>> Compound

let tier3Expr =
    choice
        [ stringLiteral
          numericalLiteral
          attempt namespacedName
          exprInParentheses
          listLiteral
          compound ]

let tier2Expr =
    let unaryOperators = "+-~!"

    pipe2 (anyOf unaryOperators |>> string) tier3Expr (fun op node -> Application(Operator op, node))
    <|> tier3Expr

let application =
    let mapper =
        let folder st nw =
            match st with
            | None -> Some nw
            | Some node -> Some(Application(node, nw))

        List.fold folder None >> _.Value


    pipe2 tier2Expr (many (attempt (ws1 >>? tier3Expr))) (fun f rest -> f :: rest)
    |>> mapper

let conditional: DexterParser<_> =
    pipe3
        (keyword "if" >>? ws1 >>. tier0Expr .>> anyWs1) 
        (skipString "then" >>. ws1 >>. tier0Expr .>> anyWs1)
        (skipString "else" >>. ws1 >>. tier0Expr)
        (fun x y z -> Conditional(x, y, z))
        
let case =
    (skipString "case" >>. anyWs1 >>. pattern false) .>>. (anyWs >>. skipString "->" >>. anyWs >>. tier0Expr) |>> Case        

let tier1'5Expr = conditional <|> application

let pmatch =
    (keyword "match" >>. anyWs1 >>. tier1'5Expr .>> anyWs) .>>.
    (skipChar '{' >>. anyWs >>. multiline1 case false .>> anyWs .>> skipChar '}') |>> Match

let functionArgs =
    let arg = patternInParenthesis true <|> tier1Pattern true
    
    pipe2 arg (many (ws1 >>? arg)) <| fun first rest -> first::rest

let pfunc =
    (skipString "fun" >>. anyWs1 >>. functionArgs .>> anyWs) .>>.
    (skipString "->" >>. anyWs >>. tier0Expr) |>> Function

let monadBind =
    pipe2
        (keyword "do" >>. anyWs1 >>. opt (pattern true .>>? ws .>>? skipString "<-" .>> anyWs))
        tier0Expr <|
        fun bind body -> match bind with
                         | None -> MonadBind(SkipPattern, body)
                         | Some bind' -> MonadBind(bind', body)

let monadExec =
    let applyBind stmts =
        let rec applyBind' stmts' acc =
            match stmts' with
            | [] -> Compound <| List.rev acc
            | stmt::rest ->
                match stmt with
                | MonadBind(bind, body) ->
                    let appliedRest = applyBind'  rest []
                    let bindExpr = Application(Application(Operator(">>="), body), Function([bind], appliedRest))
                    applyBind' [] (bindExpr::acc)
                | _ -> applyBind' rest (stmt::acc)
        applyBind' stmts []
            
    keyword "exec" >>. anyWs >>. skipChar '{' >>. anyWs >>.
    multiline (monadBind <|> tier1Statement) false .>>
    anyWs .>> skipChar '}' |>> applyBind

let tier1Expr = tier1'5Expr <|> pmatch <|> pfunc <|> monadExec

tier0ExprRef.Value <-
    let operatorPrecedence =
        function
        | "**" -> 0.
        | "*" -> 1.
        | "/" -> 1.
        | "//" -> 1.
        | "%" -> 1.
        | "+" -> 2.
        | "-" -> 2.
        | ">>>" -> 3.
        | "<<<" -> 3.
        | "<" -> 4.
        | ">" -> 4.
        | "<=" -> 4.
        | ">=" -> 4.
        | "=" -> 5.
        | "!=" -> 5.
        | "&" -> 6.
        | "^" -> 7.
        | "|" -> 8.
        | "&&" -> 9.
        | "||" -> 10.
        | "<|" -> 11.
        | "|>" -> 12.
        | "<<" -> 13.
        | ">>" -> 13.
        | ">>=" -> 14.
        | "::" -> 15.
        | _ -> 16.

    // У операторов с одинаковым приоритетом обязана быть одинаковая ассоциативноть,
    // т.е. ассоциативность - функция от приоритета
    let associativity level =
        match level with
        | 0. -> -1
        | 15. -> -1 // Правая
        | _ -> 1 // Левая


    let formTree operators operands =
        let levels =
            SortedSet<_>(
                operators
                |> List.map (function
                    | Operator(s) -> operatorPrecedence s)
            )

        let folder (operators', operands') (precedence: float) =
            let newOperators =
                operators'
                |> List.filter (function
                    | Operator(s) -> (operatorPrecedence s) <> precedence)

            let assoc = associativity precedence
            let operatorsToTravers = if assoc = 1 then operators' else List.rev operators'
            let operandsToTraverse = if assoc = 1 then operands' else List.rev operands'

            let combiner (ops: Node list) (opdZipper: Node * Node list) =
                let rec combiner' ops' (opdZipper': Node * Node list) (acc: Node list) =
                    match ops' with
                    | [] -> (fst opdZipper') :: acc
                    | Operator(op) :: restOp ->
                        let (opd1, opd2 :: nextOpds) = opdZipper'

                        if operatorPrecedence op <> precedence then
                            combiner' restOp (opd2, nextOpds) (opd1 :: acc)
                        else
                            let combined =
                                if assoc = 1 then
                                    Application(Application(Operator(op), opd1), opd2)
                                else
                                    Application(Application(Operator(op), opd2), opd1)

                            combiner' restOp (combined, nextOpds) acc

                let res = combiner' ops opdZipper []
                if assoc = 1 then List.rev res else res

            let h :: t = operandsToTraverse
            let newOperands = combiner operatorsToTravers (h, t)
            (newOperators, newOperands)

        levels |> Seq.fold folder (operators, operands) |> snd |> Seq.head

    let mapper (first, lst) =
        let operators, tailOperands = List.unzip lst
        let operands = first :: tailOperands

        match operators with
        | [] -> first
        | _ -> formTree operators operands
    tier1Expr .>>. many ((attempt ws1 >>? operator .>> anyWs) .>>. tier1Expr) |>> mapper

let expression = tier0Expr

let equation allowOperator =
    let allowedName = if allowOperator then plainName <|> prefixOperator else plainName
    let lhs = (allowedName .>>. (anyWs1 >>. opt functionArgs) <|> (pattern true |>> fun x -> (x, None)))
    pipe2
        (keyword "let" >>. anyWs1 >>. lhs .>> anyWs)
        (skipChar '=' >>. anyWs >>. expression) <|
        fun (name, args) body ->
            match args with
            | None -> Equation(name, body)
            | Some [] -> Equation(name, body)
            | Some args' -> Equation(name, Function(args', body))

let constructor =
    pipe2
        plainName <|
        opt (anyWs >>? skipChar '(' >>. anyWs >>. commaSeparated plainName false false .>> anyWs .>> skipChar ')') <|
        fun name args -> match args with
                         | None -> Constructor(name, [])
                         | Some args' -> Constructor(name, args')

let typeMembers =
    skipChar '{' >>. anyWs >>. multiline (equation true) false .>> anyWs .>> skipChar '}'

let ptype =
    pipe3
        (keyword "type" >>. anyWs1 >>. plainName .>> anyWs)
        (skipChar ':' >>. anyWs >>. commaSeparated constructor false false .>> anyWs)
        typeMembers <|
        fun name cons members -> Type(name, cons, members)

let eval =
    keyword "eval" >>. anyWs1 >>. expression |>> Eval
        
tier1StatementRef.Value <-
    equation false <|> ptype <|> eval <|> expression

let entrypoint =
    keyword "entrypoint" >>. anyWs >>. skipChar '=' >>. anyWs >>. expression |>> Entrypoint

let import =
    let source = stringLiteral |>> (function StringLiteral(s) -> s)
    let targets = ((skipChar '*' >>% []) <|> commaSeparated namespacedName true false) .>> anyWs .>> keyword "from" .>> anyWs
    
    keyword "import" >>. anyWs >>. pipe2 (opt targets) source
                                       (fun targets' source' -> match targets' with
                                                                | None -> ImportNamespace source'
                                                                | Some [] -> ImportAll source'
                                                                | Some lst -> ImportFrom(source', lst))
 
let statement = entrypoint <|> import <|> tier1Statement

let program = anyWs >>. multiline statement true .>> anyWs .>> eof |>> Program

