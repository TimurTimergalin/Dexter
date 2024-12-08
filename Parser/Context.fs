module Parser.Context

open FParsec

type ParserContext = unit
let initContext: ParserContext = ()
type DexterParser<'res> = Parser<'res, ParserContext>
