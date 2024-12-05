open FParsec
open Parser

[<EntryPoint>]
let main argv =
    let parser = Main.program
    let result = run parser "match x {case(y)when{y}->printf 'lol'}"

    match result with
    | Success(node, _, _) -> printfn $"S:{node}"
    | Failure(s, _, _) -> printfn $"F:{s}"

    0 // return an integer exit code
