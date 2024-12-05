module Parser.Context
open FParsec

type ParserContext = unit
type DexterParser<'res> = Parser<'res, ParserContext>
