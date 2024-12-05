open FParsec
open Parser
open Parser.Literals

[<EntryPoint>]
let main argv =
    let parser = Main.program
    let result = run parser "+(+a)"

    match result with
    | Success(node, _, _) -> printfn $"S:{node}"
    | Failure(s, _, _) -> printfn $"F:{s}"

    0 // return an integer exit code
