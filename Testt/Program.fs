open FParsec
open Parser

[<EntryPoint>]
let main argv =
    let parser = Main.program
    let result = run parser "if(2)then(1)else2"

    match result with
    | Success(node, _, _) -> printfn $"S:{node}"
    | Failure(s, _, _) -> printfn $"F:{s}"

    0 // return an integer exit code
