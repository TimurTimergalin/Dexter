module Parser.debug

open FParsec

let BP (p: Parser<_,_>) stream =
    p stream // set a breakpoint here