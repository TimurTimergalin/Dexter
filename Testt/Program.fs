open System.IO

[<EntryPoint>]
let main argv =
    let path = Path.GetFullPath(".", "c:/a.txt")
    printf $"%s{path}"
    0 // return an integer exit code
