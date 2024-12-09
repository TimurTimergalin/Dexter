open System.IO

let factorial n =
    let rec factorial' n acc = if n = 0 then acc else factorial' (n - 1) (acc * n)
    factorial' n 1

[<EntryPoint>]
let main argv =
    let x = factorial 1000
    printf $"%d{x}"
    0 // return an integer exit code
