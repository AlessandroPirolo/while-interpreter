module Program

open Interp
open System


let print_colored color text = Console.Write(sprintf "\x1b[%sm%s\x1b[0m" color text)

[<EntryPoint>]
let main argv =
    print_colored "32;0" "Loading source file...\n"
    let stmt = System.IO.File.ReadAllText argv.[0]
    let r = 
        try
            printfn "Start computing..."
            start stmt
        with e -> printfn "\n exception caught: %O" e; Map ["err", 0]
    printfn "%A" r
    0

